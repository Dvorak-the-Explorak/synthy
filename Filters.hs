{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Filters where


import Control.Monad.State
import Control.Lens

import General (Pulse, Hz, Volume, Seconds)



-- The state here probably doesn't need to be Pulse
-- maybe: 
-- data Filter a = Filter {
--   state :: a,
--   filt :: (Pulse -> State a Pulse)
-- }
data FilterState = FilterState {
  _prevOut ::Pulse, 
  _cutoff :: Hz
}
newtype Filter = Filter (Pulse -> State FilterState Pulse)
runFilter :: Filter -> Pulse -> State FilterState Pulse
runFilter (Filter f) = f
-- type FilterState = (a, a -> Pulse -> (a,Pulse))
-- type FilterState = State a Pulse
-- It's not actually a volume, but that's what the filter envelope outputs
newtype FiltEnvCurve = FiltEnvCurve (Volume -> Hz)
runFiltEnvCurve :: FiltEnvCurve -> (Volume -> Hz)
runFiltEnvCurve (FiltEnvCurve f) = f


-- makes the lenses, calls the lens for _prevOut just prevOut
makeLenses ''FilterState

-- ================================================================



hashtagNoFilter :: Filter
hashtagNoFilter = Filter $ return 

lowPass :: Seconds -> Filter
lowPass dt = Filter (\pulse -> state $ \fs -> 
    let prev = fs ^. prevOut
        freq = fs ^. cutoff
        rc = 1/(2*pi*freq)
        alpha = dt / (rc + dt)
        next = alpha*pulse +  (1-alpha) * prev
    in (next, fs & prevOut .~ next)
  )

-- ===============================================


mapFilter :: Filter -> [Pulse] -> State FilterState [Pulse]
mapFilter _filt [] = return []
mapFilter _filt (pulse:pulses) = do
  firstFiltered <- runFilter _filt pulse 
  restFiltered <- mapFilter _filt pulses
  return $ firstFiltered:restFiltered