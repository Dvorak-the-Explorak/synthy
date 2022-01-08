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
-- a Stepp



-- `s` can be stepped, taking an `a` input, and producing a `b`
class Steppable a b s where
  step :: a -> State s b

instance Steppable a b s => Steppable a [b] [s] where
  -- step :: Seconds -> State 
  step inp = stateMap $ step inp
