module Test where
import Control.Lens
-- import Control.Lens.Tuple


dupArg :: (a -> a -> b) -> (a -> b)
dupArg f x = f x x 

double :: String -> String
double = dupArg (++)

-- make `first` (Data.Tuple.Extra) from lenses
foirst :: (a -> c) -> (a, b) -> (c, b)
-- first f (x,y) = (f x, y)
foirst = over _1