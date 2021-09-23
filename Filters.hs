{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Filters where


import Control.Monad.State
import Control.Lens

import General (Pulse, Hz, Volume, Seconds)


-- #TODO replace _cutoff with something else, 
--  it doesn't make sense to change the cutoff of a reverb filter

data Filter a = Filter {
  _storage :: a, 
  _cutoff :: Hz,
  _filtFunc :: FilterFunc a
}


type FilterFunc a = (Pulse -> State (Filter a) Pulse)



-- type FilterState = (a, a -> Pulse -> (a,Pulse))
-- type FilterState = State a Pulse
-- It's not actually a volume, but that's what the filter envelope outputs
newtype FiltEnvCurve = FiltEnvCurve (Volume -> Hz)
runFiltEnvCurve :: FiltEnvCurve -> (Volume -> Hz)
runFiltEnvCurve (FiltEnvCurve f) = f


-- makes the lenses, calls the lens for _prevOut just prevOut
makeLenses ''Filter

-- ================================================================



-- #TODO some other filter types need different states to just `Pulse`
-- #TODO could make Filter be polymorphic in its internal state


hashtagNoFilter :: FilterFunc ()
hashtagNoFilter = return 

lowPass :: Seconds -> FilterFunc Pulse
lowPass dt = (\pulse -> state $ \fs -> 
  let 
    prev = fs ^. storage
    freq = fs ^. cutoff
    rc = 1/(2*pi*freq)
    alpha = dt / (rc + dt)
    next = alpha*pulse +  (1-alpha) * prev
  in (next, fs & storage .~ next)
  )

-- #TODO highPass needs prevOut AND prevIn, do some polymorphism magic
highPass :: Seconds -> FilterFunc Pulse
highPass dt = (\pulse -> state $ \fs -> 
    let 
      prev = fs ^. storage -- prev == prevOut - prevIn
      freq = fs ^. cutoff
      rc = 1/(2*pi*freq)
      alpha = rc / (rc + dt)
      next = alpha*pulse +  alpha * prev
    in (next, fs & storage .~ (next - pulse) ) 
  )

-- #TODO highPass needs prevOut AND prevIn, do some polymorphism magic
newHighPass :: Seconds -> FilterFunc (Pulse, Pulse)
newHighPass dt = (\pulse -> state $ \fs -> 
    let 
      (prevIn, prevOut) = fs ^. storage
      freq = fs ^. cutoff
      rc = 1/(2*pi*freq)
      alpha = rc / (rc + dt)
      next = alpha*pulse +  alpha * (prevOut - prevIn)
    in (next, fs & storage .~ (pulse, next) ) 
  )

-- #TODO make filters composable?  want to have bandPass lo hi = highPass lo . lowPass hi 

-- ===============================================


mapFilter :: FilterFunc a -> [Pulse] -> State (Filter a) [Pulse]
mapFilter _filt [] = return []
mapFilter _filt (pulse:pulses) = do
  firstFiltered <- _filt pulse 
  restFiltered <- mapFilter _filt pulses
  return $ firstFiltered:restFiltered