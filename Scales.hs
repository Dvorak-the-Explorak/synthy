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
scaleFromIntervals = scaleFromIntervalsTET

scaleFromIntervalsTET :: [Int] -> Scale
scaleFromIntervalsTET gaps n | n < 0 = 1.0/(scaleFromIntervalsTET (reverse gaps) (-n))
                             | n == 0 = 1.0
                             | n < 7 = (2 ** (1.0/(fromIntegral $ sum gaps))) ** (fromIntegral (scanl1 (+) gaps !! (n-1)))
                             | otherwise = 2 * (scaleFromIntervalsTET gaps $ n-7)


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


thiccIonian :: Scale
thiccIonian = scaleFromIntervalsTET [1,1,0,1,1,1,0]

ionian19TET :: Scale
ionian19TET = scaleFromIntervalsTET $ map (+1) [2, 2, 1, 2, 2, 2, 1]

ionian26TET :: Scale
ionian26TET = scaleFromIntervalsTET $ map (+2) [2, 2, 1, 2, 2, 2, 1]

sevenToneEqualTemp :: Scale
sevenToneEqualTemp = scaleFromIntervalsTET [1,1,1,1,1,1,1]