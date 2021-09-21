{-# LANGUAGE RankNTypes #-}

module Helpers where

import Prelude hiding (unzip)
import Control.Monad.State
import Data.Tuple.Extra
import Control.Functor.HT (unzip)
import Control.Lens
import Control.Lens.Lens
  
rot :: Int -> [a] -> [a]
rot _ [] = []
rot 0 xs = xs
rot n (x:xs) = rot (n-1) $ xs ++ [x]

mapWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhere p f = map (\x -> if (p x) then f x else x)


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = fmap . fmap



-- State things
-- ==================================


idState :: State a ()
idState = return ()

pairStatesWith ::  (a -> b -> c) -> State s1 a -> State s2 b -> State (s1, s2) c
pairStatesWith f op1 op2 = joinStatesWith f (firstState op1) (secondState op2)
    -- -- I just liked this implementation.  All the stars
    -- pairStatesWith ::  (a -> b -> c) -> State s1 a -> State s2 b -> State (s1, s2) c
    -- pairStatesWith f op1 op2 = state $ \s -> let
    --   result = runState op1 *** runState op2 $ s
    --   out = uncurry f . (fst *** fst) $ result
    --   s' = snd *** snd $ result
    --   in (out, s')

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

firstState :: State s1 a -> State (s1, s2) a
firstState op = overState _1 op
    -- firstState :: State s1 a -> State (s1, s2) a
    -- firstState op = state $ \s ->
    --   let (x, s1) = runState op $ view _1 s
    --   in (x, set _1 s1 s)
        -- firstState :: State s1 a -> State (s1,s2) a
        -- firstState op = state $ \(s1,s2) -> 
        --     let (x, s1') = runState op s1
        --     in (x, (s1', s2))


secondState :: State s2 a -> State (s1,s2) a
secondState op = overState _2 op
    -- secondState :: State s2 a -> State (s1,s2) a
    -- secondState op = state $ \s -> 
    --   let (x, s2) = runState op $ view _2 s
    --   in (x, set _2 s2 s)
        -- secondState :: State s2 a -> State (s1,s2) a
        -- secondState op = state $ \(s1,s2) -> 
        --     let (x, s2') = runState op s2
        --     in (x, (s1, s2'))

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
