{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , ExistentialQuantification
           , FlexibleInstances
           , RankNTypes
  #-}

module Filters where

-- A Filter is a stateful object which can modify sequences of pulses 
--  it has internal state, and exposes parameters.  

import Control.Monad.State
import Control.Lens

import General (Pulse, Hz, Volume, Seconds)
import Steppable
import Helpers
import Parameterised


-- type variable a indicates what parameters the filter exposes
-- type variable s is hidden inside the filter (Like ST monad?), 
--    and cannot escape to the outside world
data Filter a = forall s. Filter {
  _filterStorage :: s,
  _filterParam :: a,
  _filterRun :: FilterFunc s a
}






-- the only thing that's allowed to touch the filter storage type
--  takes its parameter type and the input pulse,
--  returns statefult effect that updates the filterStorage and returns output pulse
type FilterFunc s a = (a -> Pulse -> State s Pulse)

-- makes the lenses, calls the lens for _filterParam just param
makeFields ''Filter



-- This is made slightly messier because we can't use record accesors or record updates
--  means lenses don't work either, have to make the whole record at once / pattern match
instance Steppable Pulse Pulse (Filter a) where
  step pulse = state $ \(Filter s param run) -> let 
      (output, s') = runState (run param pulse) s
    in (output, Filter s' param run)
    
instance FreqField a => FreqField (Filter a) where
  freq = param.freq

instance WaveIndexField a => WaveIndexField (Filter a) where
  waveIndex = param.waveIndex

-- apply a function to the filter output
mapFilterOutput :: (Pulse -> Pulse) -> Filter a -> Filter a
mapFilterOutput f (Filter s p r) = let 
    r' = \p' pulse -> fmap f (r p' pulse)
  in (Filter s p r')    

(~>) :: Filter a -> Filter b -> Filter (a,b)
(~>) = sequenceFiltersPacked id id

(+>) :: Filter a -> Filter b -> Filter (a,b)
(+>) = parallelFilters (+)

(*>) :: Filter a -> Filter b -> Filter (a,b)
(*>) = parallelFilters (*)





parallelFilters :: (Pulse -> Pulse -> Pulse) -> Filter a -> Filter b -> Filter (a,b)
parallelFilters f = parallelFiltersPacked f id id

-- packs and unpacks the parameters to a different form
parallelFiltersPacked :: (Pulse -> Pulse -> Pulse) -> ((a,b) -> c) -> (c -> (a,b)) -> Filter a -> Filter b -> Filter c
parallelFiltersPacked f packParams getParams (Filter s1 p1 r1) (Filter s2 p2 r2) = let    
    s = (s1, s2)
    p = packParams (p1, p2)
    r = \p' pulse -> do
      let (p1', p2') = getParams p'
      out1 <- overState _1 $ r1 p1' pulse
      out2 <- overState _2 $ r2 p2' pulse
      return $ f out1 out2
  in Filter s p r

sequenceFiltersPacked :: ((a,b) -> c) -> (c -> (a,b)) -> Filter a -> Filter b -> Filter c
sequenceFiltersPacked packParams getParams (Filter s1 p1 r1) (Filter s2 p2 r2) = let
    s = (s1,s2)
    p = packParams (p1, p2)
    r = \p' pulse -> do
      let (p1', p2') = getParams p'
      out1 <- overState _1 $ r1 p1' pulse
      out2 <- overState _2 $ r2 p2' out1
      return out2
  in Filter s p r

joinFilters :: ((Pulse, Pulse) -> Pulse) -> ((Pulse, Pulse) -> Pulse) -> Filter a -> Filter b -> Filter (a,b)
joinFilters secondInput getResult (Filter s1 p1 r1) (Filter s2 p2 r2) = let
    s = (s1, s2)
    p = (p1, p2)
    r = \(p1', p2') pulse -> do
      out1 <- overState _1 $ r1 p1' pulse
      out2 <- overState _2 $ r2 p2' $ secondInput (pulse,out1)
      return $ getResult (out1,out2)
  in (Filter s p r)

-- makes functionally identical representations while refactoring
kernelToFilter :: Kernel (WithStorage s a) Pulse Pulse -> Filter a
kernelToFilter (Kernel s@(WithStorage store param) go) = Filter store param run
  where
    -- go :: Pulse -> State (s,a) Pulse
    -- run :: a -> Pulse -> State s Pulse
    -- run :: a -> Pulse -> State s Pulse
    run param pulse = state $ \store -> let 
        (out, WithStorage store' param') = runState (go pulse) s
        -- ignore the modification to param, it shouldn't change in step
      in (out, store')
-- can't undo k2f, because the storage type is hidden in Filter




-- ===============================)=================================

bandPass :: Seconds -> Filter (FreqParam,FreqParam)
bandPass dt = highPass dt ~> lowPass dt

centeredBandPass :: Seconds -> Filter (FreqParam,FreqParam)
centeredBandPass dt = let 
    packParam = \(FreqParam lo, FreqParam hi) -> (FreqParam $ (lo+hi)/2, FreqParam $ hi-lo)
    getParam = \(FreqParam center, FreqParam bandwidth) -> (FreqParam $ center-bandwidth, FreqParam $ center+bandwidth)
  in sequenceFiltersPacked packParam getParam (highPass dt) (lowPass dt)

lowPass :: Seconds -> Filter FreqParam 
lowPass = kernelToFilter . lowPass2
-- lowPass dt = Filter {
--   _filterStorage = 0,
--   _filterParam = FreqParam 0,
--   _filterRun = lowPassFunc dt
-- }

lowPass2 :: Seconds -> Kernel (WithStorage Pulse FreqParam) Pulse Pulse
lowPass2 dt = Kernel s go
  where
    s = WithStorage 0 (FreqParam 0)
    go = lowPassFunc2 dt


highPass :: Seconds -> Filter FreqParam
highPass dt = Filter {
  _filterStorage = (0,0),
  _filterParam = FreqParam 0,
  _filterRun = highPassFunc dt
}

hashtagNoFilter :: a -> Filter a
hashtagNoFilter param = Filter {
  _filterStorage = (),
  _filterParam = param,
  _filterRun = hashtagNoFilterFunc
}

combFilter :: Filter (Float,Int)
combFilter = Filter {
  _filterStorage = [],
  _filterParam = (0.8, 10),
  _filterRun = combFilterFunc
}

clipper :: Filter Volume
clipper = Filter () (1) clipperFunc

-- -- has no internal state, just applies a given function
pureFilter :: (Pulse -> Pulse) -> Filter ()
pureFilter f = Filter () () (const $ return . f)


-- cubicFilter :: Filter ()
-- cubicFilter = pureFilter (**3)
cubicFilter :: Filter Float
cubicFilter = Filter () 1 (\strength pulse -> return $ strength*pulse**3 + (1-strength)*pulse)

gainFilter :: Filter Float
gainFilter = Filter () 1 (\gain -> return . (*gain)) 

-- ================================

hashtagNoFilterFunc :: FilterFunc () a
-- hashtagNoFilterFunc _ = return 
hashtagNoFilterFunc = const return 

rcFromCutoff :: Hz -> Float
rcFromCutoff f = 1/(2*pi*f)

lowPassFunc :: Seconds -> FilterFunc Pulse FreqParam
lowPassFunc dt = (\(FreqParam cutoff) pulse -> state $ \prev -> 
  let 
    rc = 1/(2*pi*cutoff)
    alpha = dt / (rc + dt)
    next = alpha*pulse +  (1-alpha) * prev
  in (next, next)
  )

-- --                         (--------------- Kernel go function -------------) 
-- lowPassFunc2 :: Seconds -> Pulse -> State (ParamSecond Pulse FreqParam) Pulse
-- lowPassFunc2 dt = \pulse -> state $ \(ParamSecond (prev, FreqParam cutoff)) -> 
--   let 
--     rc = 1/(2*pi* cutoff)
--     alpha = dt / (rc + dt)
--     next = alpha*pulse +  (1-alpha) * prev
--   in (next, ParamSecond (next, FreqParam cutoff))

  --                       (--------------- Kernel go function -------------) 
lowPassFunc2 :: Seconds -> Pulse -> State (WithStorage Pulse FreqParam) Pulse
lowPassFunc2 dt = \pulse -> do
    WithStorage prev (FreqParam _freq) <- get
    let rc = rcFromCutoff _freq 
    let alpha = dt / (rc + dt)
    storage %= (alpha * pulse +) . ((1-alpha) *)
    use storage



highPassFunc :: Seconds -> FilterFunc (Pulse,Pulse) FreqParam
highPassFunc dt = (\(FreqParam cutoff) pulse -> state $ \(prevOut, prevIn) -> 
    let 
      rc = 1/(2*pi*cutoff)
      alpha = rc / (rc + dt)
      next = alpha*pulse +  alpha * (prevOut - prevIn)
    in (next, (next, pulse) ) 
  )


-- Should this take delay as a Seconds parameter instead of samples?
combFilterFunc :: FilterFunc [Pulse] (Float,Int)
combFilterFunc = (\(strength, delay) pulse -> state $ \history ->
    let
      n = length history
      next = if null history
              then pulse
              else pulse + strength * (head history)
      history' = if n < delay
                  then history ++ [next]
                  else (drop (n-delay+1) $ history) ++ [next]
    in (next, history')
  )


clipperFunc :: FilterFunc () Volume
-- clipperFunc = \limit pulse-> state $ \() -> (hardClipLimit limit pulse, ())
-- clipperFunc = \limit pulse-> return $ hardClipLimit limit pulse 
-- clipperFunc = return .: hardClipLimit -- no gain
clipperFunc = \limit ->  return . (/limit) . hardClipLimit limit



-- ===============================================

mapFilter :: FilterFunc s p -> p -> [Pulse] -> State s [Pulse]
mapFilter _filt param [] = return []
mapFilter _filt param (pulse:pulses) = do
  firstFiltered <- _filt param pulse 
  restFiltered <- mapFilter _filt param pulses
  return $ firstFiltered:restFiltered