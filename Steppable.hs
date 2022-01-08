{-# LANGUAGE MultiParamTypeClasses
             , RankNTypes
             , ExistentialQuantification 
             , FlexibleInstances
             , FlexibleContexts
             , ConstraintKinds #-}
module Steppable where


import Control.Monad.State (State(..), runState, state)
import Control.Lens
import Data.Tuple.Extra (first, second)

import General
import Helpers

-- The steppable typeclass represents things that can be stepped
--  ie. have some internal state, and a function to produce a stateful operation on themselves



-- s can be stepped, producing an a
class Steppable a s where
  step :: Seconds -> State s a

instance Steppable a s => Steppable [a] [s] where
  -- step :: Seconds -> State 
  step dt = stateMap $ step dt