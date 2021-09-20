module Test where

import Control.Lens.Tuple

main = putStrLn $ show $ first double ("hello", "world")


dupArg :: (a -> a -> b) -> (a -> b)
dupArg f x = f x x 

double :: String -> String
double = dupArg (++)

first :: (a -> c) -> (a, b) -> (c, b)
-- first f (x,y) = (f x, y)
first = over _1