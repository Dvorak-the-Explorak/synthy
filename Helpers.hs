{-# 
    LANGUAGE RankNTypes
    , MultiParamTypeClasses
 #-}

module Helpers where

import Prelude hiding (unzip)
import Control.Monad.State
import Data.Tuple.Extra
import Control.Functor.HT (unzip)
import Control.Lens
import Control.Lens.Lens


import General (Pulse)

  
rot :: Int -> [a] -> [a]
rot _ [] = []
rot 0 xs = xs
rot n (x:xs) = rot (n-1) $ xs ++ [x]

group :: Int -> [a] -> [[a]]
group 0 _ = []
group n xs = take n xs : (group n $ drop n xs)

mapWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhere p f = map (\x -> if (p x) then f x else x)


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = fmap . fmap -- using the (a->) functor
-- fmap (c -> d) :: Functor f => f c -> f d 
-- fmap (c->d) :: (b->) c -> (b->) d
-- fmap (c->d) :: (b->c) -> (b->d)
-- fmap (fmap (c->d)) :: Functor f => f (b->c) -> f (b->d)
-- fmap (fmap (c->d)) :: (a->) (b->c) -> (a->) (b->d)
-- fmap (fmap (c->d)) :: (a->b->c) -> (a->b->d)
-- fmap . fmap :: (c->d) -> (a->b->c) -> (a->b->d)
-- lovely



hardClip :: Pulse -> Pulse
hardClip = (max (-1.0)) . (min 1.0)

-- State things
-- ==================================


idState :: State a ()
idState = return ()

joinStatesWith :: (a -> b -> c) -> State s a -> State s b -> State s c
joinStatesWith f op1 op2 = do
  out1 <- op1
  out2 <- op2
  return $ f out1 out2

-- overState :: Lens s s a a -> State s b -> State a b
-- overState = undefined
overState :: Lens s s a a -> State a b -> State s b
overState l op = state $ \s -> 
  let (output, subState) = runState op $ view l s
  in (output, set l subState s)

-- from Prelude or Data.List
-- unzip :: [(a,b)] -> ([a], [b])
stateMap :: State s a -> State [s] [a]
stateMap op = state $ unzip . map (runState op)

-- from Data.Functor.HT
-- unzip :: Function f => f (a,b) -> (f a, f b)
injectState :: Functor f => State s a -> State (f s) (f a)
injectState op = state $ unzip . fmap (runState op)

injectWith :: Functor f => (f a -> a) -> State s a -> State (f s) a
injectWith combine op = state $ first combine . unzip . fmap (runState op)
