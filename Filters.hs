{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , ExistentialQuantification
           , FlexibleInstances
           , RankNTypes
  #-}

module Filters where


import Control.Monad.State
import Control.Lens

import General (Pulse, Hz, Volume, Seconds)
import Helpers


-- #TODO make filters composable instead of needing sequenceFilters - want to have bandPass lo hi = highPass lo . lowPass hi 


-- type variable a indicates what parameters the filter exposes
data Filter a = forall s. Filter {
  _filterStorage :: s,
  _filterParam :: a,
  _filterRun :: FilterFunc s a
}
type FilterFunc s a = (a -> Pulse -> State s Pulse)

-- makes the lenses, calls the lens for _filterStorage just storage
makeFields ''Filter


-- so much for records and lenses taking the mess out of it
--  I can't even goddamn look at this, and it's barely a line of lenses
runFilter pulse (Filter {_filterStorage=store, _filterParam=param, _filterRun=run}) = let
  (output, newStore) = runState (run param pulse) store
  in (output, Filter {_filterStorage=newStore, _filterParam=param, _filterRun=run})

(~>) :: Filter a -> Filter b -> Filter (a,b)
(~>) = sequenceFiltersPacked id id

(+>) :: Filter a -> Filter b -> Filter (a,b)
(+>) = parallelFilters (+)

(*>) :: Filter a -> Filter b -> Filter (a,b)
(*>) = parallelFilters (*)


parallelFilters :: (Pulse -> Pulse -> Pulse) -> Filter a -> Filter b -> Filter (a,b)
parallelFilters f = parallelFiltersPacked f id id

parallelFiltersPacked :: (Pulse -> Pulse -> Pulse) -> ((a,b) -> c) -> (c -> (a,b)) -> Filter a -> Filter b -> Filter c
parallelFiltersPacked f packParams getParams (Filter {_filterStorage=s1, _filterParam=p1, _filterRun=r1}) (Filter {_filterStorage=s2, _filterParam=p2, _filterRun=r2}) = let    
    r = \p' pulse -> 
          state $ \(s1', s2') -> 
            let (p1', p2') = getParams p'
                -- do both the filters on the input pulse
                (out1, newS1) = runState (r1 p1' pulse) s1'
                (out2, newS2) = runState (r2 p2' pulse) s2'
            in (f out1 out2, (newS1, newS2))
  in (Filter {_filterStorage=(s1,s2), _filterParam=packParams (p1,p2), _filterRun=r})    

sequenceFiltersPacked :: ((a,b) -> c) -> (c -> (a,b)) -> Filter a -> Filter b -> Filter c
sequenceFiltersPacked packParams getParams (Filter {_filterStorage=s1, _filterParam=p1, _filterRun=r1}) (Filter {_filterStorage=s2, _filterParam=p2, _filterRun=r2}) = let    
    -- r1 :: a -> Pulse -> State s1 Pulse
    -- r2 :: b -> Pulse -> State s2 Pulse
    r = \p' pulse -> 
          state $ \(s1', s2') -> 
            let (p1', p2') = getParams p'
                -- run first filter on input pulse
                (out1, newS1) = runState (r1 p1' pulse) s1'
                -- run second filter on output of first filter
                (out2, newS2) = runState (r2 p2' out1) s2'
            in (out2, (newS1, newS2))
  in (Filter {_filterStorage=(s1,s2), _filterParam= packParams (p1,p2), _filterRun=r})

-- ================================================================

bandPass :: Seconds -> Filter (Hz,Hz)
bandPass dt = highPass dt ~> lowPass dt
-- bandPass dt = sequenceFilters (highPass dt) (lowPass dt)

centeredBandPass :: Seconds -> Filter (Hz,Hz)
centeredBandPass dt = let 
    packParam = \(lo, hi) -> ((lo+hi)/2, hi-lo)
    getParam = \(center, bandwidth) -> (center-bandwidth, center+bandwidth)
  in sequenceFiltersPacked packParam getParam (highPass dt) (lowPass dt)

lowPass :: Seconds -> Filter Hz 
lowPass dt = Filter {
  _filterStorage = 0,
  _filterParam = 0,
  _filterRun = lowPassFunc dt
}

highPass :: Seconds -> Filter Hz
highPass dt = Filter {
  _filterStorage = (0,0),
  _filterParam = 0,
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


-- ================================

hashtagNoFilterFunc :: FilterFunc () a
hashtagNoFilterFunc _ = return 

lowPassFunc :: Seconds -> FilterFunc Pulse Hz
lowPassFunc dt = (\cutoff pulse -> state $ \prev -> 
  let 
    rc = 1/(2*pi*cutoff)
    alpha = dt / (rc + dt)
    next = alpha*pulse +  (1-alpha) * prev
  in (next, next)
  )


highPassFunc :: Seconds -> FilterFunc (Pulse,Pulse) Hz
highPassFunc dt = (\cutoff pulse -> state $ \(prevOut, prevIn) -> 
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

-- ===============================================


mapFilter :: FilterFunc s p -> p -> [Pulse] -> State s [Pulse]
mapFilter _filt param [] = return []
mapFilter _filt param (pulse:pulses) = do
  firstFiltered <- _filt param pulse 
  restFiltered <- mapFilter _filt param pulses
  return $ firstFiltered:restFiltered