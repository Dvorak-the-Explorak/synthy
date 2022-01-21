{-# LANGUAGE MultiParamTypeClasses
            , RankNTypes
            , ExistentialQuantification 
            , FlexibleInstances
            , FlexibleContexts
            , TemplateHaskell
            , ConstraintKinds
            , FunctionalDependencies #-}
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
class Steppable i o s | i s -> o where
  step :: i -> State s o
  stepChunk :: [i] -> State s [o]
  stepChunk = mapM step

type Source s = Steppable Seconds Pulse s
type Transformer s = Steppable Pulse Pulse s




data Kernel s i o = Kernel 
  { _kernelStorage :: s
  , _kernelDoStep :: i -> State s o}

-- instance Profunctor (Kernel s) where
--   lmap f (Kernel s go) = Kernel s (go . f)
--   rmap f (Kernel s go) = Kernel s (\inp -> f <$> go inp)


-- resulting Kernel only exposes the parameters that BOTH the inputs expose
seqKernels :: Kernel s1 i1 int -> Kernel s2 int o2 -> Kernel (s1,s2) i1 o2
seqKernels = seqKernelsWith (\ s1 s2 -> (s1,s2))

-- sequence 2 kernels, using a given packing type that exposes the inputs via the _1 and _2 lenses
seqKernelsWith :: (Field1 s s s1 s1, Field2 s s s2 s2) =>
                  (s1->s2->s) -> Kernel s1 i1 int -> Kernel s2 int o2 -> Kernel s i1 o2
seqKernelsWith pack (Kernel s1 go1) (Kernel s2 go2) = (Kernel s go)
  where
    s = pack s1 s2

    go i1 = (go1 i1 .@ _1) >>= (.@ _2) . go2

    -- go i1 = do
    --   int <- go1 i1 .@ _1
    --   go2 int .@ _2



-- makes the lenses, calls the lens for _kernelStorage just storage
makeFields ''Kernel


-- this lifts the action on the Kernel's storage into an action on the Kernel,
--  and sugars it so you don't need to extract it and run it etc (just go `step input kernel`)
instance Steppable i o (Kernel s i o) where
  step inp = state $ \(Kernel _store _doStep) -> let
      (out,_store') = runState (_doStep inp) _store
    in (out, Kernel _store' _doStep)
