{-# LANGUAGE MultiParamTypeClasses
            , RankNTypes
            , ExistentialQuantification 
            , FlexibleInstances
            , FlexibleContexts
            , TemplateHaskell
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

type Source s = Steppable Seconds Pulse s
type Transformer s = Steppable Pulse Pulse s

instance Steppable a b s => Steppable a [b] [s] where
  -- step :: Seconds -> State 
  step inp = stateMap $ step inp




data Kernel s i o = Kernel 
  { _storage :: s
  , _doStep :: i -> State s o}


-- makes the lenses, calls the lens for _storage just storage
makeLenses ''Kernel


-- this lifts the action on the Kernel's storage into an action on the Kernel,
--  and sugars it so you don't need to extract it and run it etc (just go `step input kernel`)
instance Steppable i o (Kernel s i o) where
  step inp = state $ \(Kernel _store _doStep) -> let
      (out,_store') = runState (_doStep inp) _store
    in (out, Kernel _store' _doStep)
