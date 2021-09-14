module Scales where

import General
import Helpers

-- -- more like [Interval] -> Scale
-- scaleFromIntervals :: [Int] -> Scale
-- scaleFromIntervals gaps n | n < 0 = 1.0/(scaleFromIntervals (reverse gaps) (-n))
--              | n == 0 = 1.0
--              | n < 7 = semitone ** (fromIntegral (scanl1 (+) gaps !! (n-1)))
--              | otherwise = 2 * (scaleFromIntervals gaps $ n-7)
             

scaleFromIntervals :: [Int] -> Scale
scaleFromIntervals = scaleFromIntervalsTET 12

scaleFromIntervalsTET :: Int -> [Int] -> Scale
scaleFromIntervalsTET temperament gaps n | n < 0 = 1.0/(scaleFromIntervalsTET temperament (reverse gaps) (-n))
                                       | n == 0 = 1.0
                                       | n < 7 = (2 ** (1.0/(fromIntegral temperament))) ** (fromIntegral (scanl1 (+) gaps !! (n-1)))
                                       | otherwise = 2 * (scaleFromIntervalsTET temperament gaps $ n-7)


majorScale :: Scale
majorScale = ionian
minorScale :: Scale
minorScale = dorian
ionian :: Scale
ionian = scaleFromIntervals [2, 2, 1, 2, 2, 2, 1]
dorian :: Scale
dorian = scaleFromIntervals $ rot 1 [2, 2, 1, 2, 2, 2, 1]
phrygian :: Scale
phrygian = scaleFromIntervals $ rot 2 [2, 2, 1, 2, 2, 2, 1]
lydian :: Scale
lydian = scaleFromIntervals $ rot 3 [2, 2, 1, 2, 2, 2, 1]
mixolydian :: Scale
mixolydian = scaleFromIntervals $ rot 4 [2, 2, 1, 2, 2, 2, 1]
aeolian :: Scale
aeolian = scaleFromIntervals $ rot 5 [2, 2, 1, 2, 2, 2, 1]
locrian :: Scale
locrian = scaleFromIntervals $ rot 6 [2, 2, 1, 2, 2, 2, 1]


ionian19TET :: Scale
ionian19TET = scaleFromIntervalsTET 19 $ map (+1) [2, 2, 1, 2, 2, 2, 1]