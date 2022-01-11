{-# 
    LANGUAGE RankNTypes
    , MultiParamTypeClasses
    , FlexibleInstances
    , ExistentialQuantification
 #-}

module Helpers where

import Prelude hiding (unzip)
import Control.Monad.State
import Control.Monad.Reader
import Data.Tuple.Extra
import Control.Functor.HT (unzip)
import Control.Lens
import Control.Lens.Lens


import General (Pulse, Volume)

-- rotate a list, sending the first n items to the back 
rot :: Int -> [a] -> [a]
rot _ [] = []
rot 0 xs = xs
rot n (x:xs) = rot (n-1) $ xs ++ [x]

-- chunk the list into groups of n items
group :: Int -> [a] -> [[a]]
group 0 _ = []
group _ [] = []
group n xs = take n xs : (group n $ drop n xs)

-- map all elements that satisfy a boolean
mapWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhere p f = map (\x -> if (p x) then f x else x)



-- 
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
-- hardClip = (max (-1.0)) . (min 1.0)
hardClip = hardClipLimit 1


hardClipLimit :: Volume -> Pulse -> Pulse
hardClipLimit lim = (max (-lim)) . (min lim)

-- State things
-- ==================================


idState :: State a ()
idState = return ()

joinStatesWith :: (a -> b -> c) -> State s a -> State s b -> State s c
joinStatesWith f op1 op2 = do
  out1 <- op1
  out2 <- op2
  return $ f out1 out2

-- #TODO should this be a more general lens?
overState :: Lens' s a -> State a b -> State s b
overState l op = state $ \s -> 
  let (output, subState) = runState op $ view l s
  in (output, set l subState s)

(.@) :: State a b -> Lens' s a -> State s b
(.@) op l = overState l op

-- from Prelude or Data.List
-- unzip :: [(a,b)] -> ([a], [b])
-- stateMap :: State s a -> State [s] [a]
-- stateMap op = state $ unzip . map (runState op)
stateMap :: Functor f => State s a -> State (f s) (f a)
stateMap = injectState

-- from Data.Functor.HT
-- unzip :: Function f => f (a,b) -> (f a, f b)
injectState :: Functor f => State s a -> State (f s) (f a)
injectState op = state $ unzip . fmap (runState op)

injectWith :: Functor f => (f a -> a) -> State s a -> State (f s) a
injectWith combine op = state $ first combine . unzip . fmap (runState op)


iterateStateUntil :: State s Bool -> State s a -> State s [a]
iterateStateUntil cond op = do
  done <- cond
  if done 
    then return []
    else do
      x <- op
      xs <- iterateStateUntil cond op
      return $ x:xs

iterateState :: Int -> State s a -> State s [a]
iterateState 0 _ = return []
iterateState n op = do
  x <- op
  xs <- iterateState (n-1) op
  return $ x:xs


-- ==================================

-- (.=) :: Lens' s a -> a -> State s ()
-- (%=) :: Lens' s a -> (a -> a) -> State s ()

-- (.*=) :: Lens' s (a -> b) -> Lens' s a -> Lens' s b -> State s ()
-- (.*=) fl xl tl = do
--   f <- use fl
--   x <- use xl
--   tl .= f x


-- mapLens :: Lens' s a -> Lens' [s] [a]
-- mapLens l = lens get_ set_
--   where
--     -- get_ :: [s] -> [a]    
--     -- get l :: s -> a
--     get_ = map (get l)
--     -- set_ :: [s] -> [a] -> [s]
--     -- set l :: s -> a -> s
--     -- map (set l) :: [s] -> [a -> s]
--     -- something :: [a -> s] -> [a] -> [s]
--     set_ ss as = zipWith ($) (map (set l) ss) as

