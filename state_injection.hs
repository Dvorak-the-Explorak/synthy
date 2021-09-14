import Control.Monad.State

main = do


  putStrLn $ show $ take 10 $ iterateState (stepCountersWith sum) [(0,1), (0,10), (0,3)]

setCounter :: Int -> Int -> State (Int, Int) (Int)
setCounter start step = state $ const (start, (start, step))

stepCounter :: State (Int,Int) Int
stepCounter = state $ \(curr, step) -> (curr, ( curr+step, step))


setCounters :: [Int] -> [Int] -> State [(Int, Int)] [Int] 
setCounters starts steps = state $ const (starts, zip starts steps)


enlist :: State s a -> State [s] [a]
enlist op = state $ \s -> unzip $ map (runState op) s


setCounters' :: [(Int,Int)] -> State [(Int, Int)] [Int]
setCounters' states = state $ const (map fst states, states)

-- stepCounters :: State [(Int, Int)] [Int]
-- stepCounters = state $ \states -> let
--   results :: [(Int, Int)]
--   results = map (runState stepCounter) states
--   outputs = map fst results
--   newStates = map snd results
--   in  (outputs, newStates)
stepCounters :: State [(Int, Int)] [Int]
-- stepCounters = state $ \states -> unzip $ map (runState stepCounter) states
stepCounters = enlist stepCounter

stepCountersWith :: ([Int] -> a) -> State [(Int, Int)] a
stepCountersWith f = do
  outputs <- stepCounters
  return $ f outputs

-- iterateState :: State s a -> s -> [a]
-- iterateState op s = x : (iterateState op s')
--   where
--     (x,s') = runState op s
iterateState :: State s a -> s -> [a]
iterateState op s = map fst $ iterate (runState op . snd) $ runState op s

-- iteratedState :: State s a -> State s [a]
-- iteratedState op = state $ \s -> second last $ unzip $ iterate (runState op . snd) $ runState op s

second :: (b -> c) -> (a,b) -> (a,c)
second f (x,y) = (x, f y)

first :: (a -> c) -> (a,b) -> (c,b)
first f (x,y) = (f x, y)

summedCounters :: [(Int, Int)] -> [Int]
summedCounters [] = repeat 0
summedCounters states = result
  where
    next s = runState (stepCountersWith sum) s

    result = fst $ runState three states
    three = do
      starts <- stepCountersWith sum
      seconds <- stepCountersWith sum
      thirds <- stepCountersWith sum
      return [starts, seconds, thirds]









