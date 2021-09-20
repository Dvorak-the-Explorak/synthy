module Helpers where

import Prelude hiding (unzip)
import Control.Monad.State
import Data.Tuple.Extra
import Control.Functor.HT (unzip)
  
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

-- joinStatesWith :: (a -> b -> c) -> State s1 a -> State s2 b -> State (s1, s2) c
-- joinStatesWith f op1 op2 = state $ \(s1, s2) -> let
--     (out1, s1') = runState op1 s1
--     (out2, s2') = runState op2 s2
--     in (f out1 out2, (s1', s2'))

idState :: State a ()
-- idState = state $ \x -> ((), x)
idState = modify id

pairStatesWith ::  (a -> b -> c) -> State s1 a -> State s2 b -> State (s1, s2) c
pairStatesWith f op1 op2 = state $ \s -> let
    result = runState op1 *** runState op2 $ s
    out = uncurry f . (fst *** fst) $ result
    s' = snd *** snd $ result
    in (out, s')

firstState :: State s1 a -> State (s1,s2) a
firstState op = state $ \(s1,s2) -> 
    let (x, s1') = runState op s1
    in (x, (s1', s2))

secondState :: State s2 a -> State (s1,s2) a
secondState op = state $ \(s1,s2) -> 
    let (x, s2') = runState op s2
    in (x, (s1, s2'))


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
