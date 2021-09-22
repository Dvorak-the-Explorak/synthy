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

data Filter = Filter {
  _prevOut ::Pulse, 
  _cutoff :: Hz,
  _filtFunc :: FilterFunc
}

type FilterFunc = (Pulse -> State Filter Pulse)

-- type FilterState = (a, a -> Pulse -> (a,Pulse))
-- type FilterState = State a Pulse
-- It's not actually a volume, but that's what the filter envelope outputs
newtype FiltEnvCurve = FiltEnvCurve (Volume -> Hz)
runFiltEnvCurve :: FiltEnvCurve -> (Volume -> Hz)
runFiltEnvCurve (FiltEnvCurve f) = f


-- makes the lenses, calls the lens for _prevOut just prevOut
makeLenses ''Filter

-- ================================================================



hashtagNoFilter :: FilterFunc
hashtagNoFilter = return 

lowPass :: Seconds -> FilterFunc
lowPass dt = (\pulse -> state $ \fs -> 
    let prev = fs ^. prevOut
        freq = fs ^. cutoff
        rc = 1/(2*pi*freq)
        alpha = dt / (rc + dt)
        next = alpha*pulse +  (1-alpha) * prev
    in (next, fs & prevOut .~ next)
  )

-- ===============================================


mapFilter :: FilterFunc -> [Pulse] -> State Filter [Pulse]
mapFilter _filt [] = return []
mapFilter _filt (pulse:pulses) = do
  firstFiltered <- _filt pulse 
  restFiltered <- mapFilter _filt pulses
  return $ firstFiltered:restFiltered