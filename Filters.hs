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


-- #TODO make filters composable?  want to have bandPass lo hi = highPass lo . lowPass hi 



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



-- ================================================================



lowPass :: Seconds -> Filter Hz 
lowPass dt = Filter {
  _filterStorage = 0,
  _filterParam = 0,
  _filterRun = lowPassFunc dt
}

oldHighPass :: Seconds -> Filter Hz
oldHighPass dt = Filter {
  _filterStorage = 0,
  _filterParam = 0,
  _filterRun = oldHighPassFunc dt
}

highPass :: Seconds -> Filter Hz
highPass dt = Filter {
  _filterStorage = (0,0),
  _filterParam = 0,
  _filterRun = highPassFunc dt
}


-- #TODO some other filter types need different states to just `Pulse`
-- #TODO could make Filter be polymorphic in its internal state


hashtagNoFilterFunc :: FilterFunc () ()
hashtagNoFilterFunc _ = return 

lowPassFunc :: Seconds -> FilterFunc Pulse Hz
lowPassFunc dt = (\cutoff pulse -> state $ \prev -> 
  let 
    rc = 1/(2*pi*cutoff)
    alpha = dt / (rc + dt)
    next = alpha*pulse +  (1-alpha) * prev
  in (next, next)
  )

-- #TODO highPass needs prevOut AND prevIn
oldHighPassFunc :: Seconds -> FilterFunc Pulse Hz
oldHighPassFunc dt = (\cutoff pulse -> state $ \prev -> 
    let 
      rc = 1/(2*pi*cutoff)
      alpha = rc / (rc + dt)
      next = alpha*pulse +  alpha * prev
    in (next, (next - pulse) ) 
  )


highPassFunc :: Seconds -> FilterFunc (Pulse,Pulse) Hz
highPassFunc dt = (\cutoff pulse -> state $ \(prevOut, prevIn) -> 
    let 
      rc = 1/(2*pi*cutoff)
      alpha = rc / (rc + dt)
      next = alpha*pulse +  alpha * (prevOut - prevIn)
    in (next, (next, pulse) ) 
  )
-- ===============================================


mapFilter :: FilterFunc s p -> p -> [Pulse] -> State s [Pulse]
mapFilter _filt param [] = return []
mapFilter _filt param (pulse:pulses) = do
  firstFiltered <- _filt param pulse 
  restFiltered <- mapFilter _filt param pulses
  return $ firstFiltered:restFiltered