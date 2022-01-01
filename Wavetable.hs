{-#  LANGUAGE BangPatterns 
#-}

module Wavetable where

import Data.WAVE
import qualified Data.Vector as V
import Data.Vector ((!))

import General
import Helpers

import Debug.Trace

-- type Wavetable = V.Vector (V.Vector Pulse)
type Wavetable = WaveIndex -> Phase -> Pulse


waveFileDescription :: WAVE -> String
waveFileDescription wav = (show channels ++ " channels, " 
                            ++ show rate ++ " fps, " 
                            ++ show bits ++ " bits/sample, "
                            ++ show frames ++ " total frames")
  where
    channels = waveNumChannels $ waveHeader wav
    rate = waveFrameRate $ waveHeader wav
    bits = waveBitsPerSample $ waveHeader wav
    frames = waveFrames $ waveHeader wav


samplesFromWave :: WAVE -> [Pulse]
samplesFromWave wav = map (realToFrac . sampleToDouble . head) $ waveSamples wav

-- load a wavetable with n samples
loadWavetable :: Int -> WAVE -> Wavetable
loadWavetable n wav = let
    !groups = group n $ samplesFromWave wav
    !vectors = map V.fromList $ filter ((==n) . length) groups
    !table = V.fromList vectors
  in readTable table

-- Given a 2D Vector of pulses, produce a Wavetable (function from index and phase to pulse)
readTable :: V.Vector (V.Vector Pulse) -> Wavetable
readTable table = \waveIndex_ phase_ -> let 
    samplesPerWave = V.length $ V.head table
    numWaves = V.length table
    i = min (floor $ phase_ * fromIntegral samplesPerWave) (samplesPerWave-1)
    waveN = min (floor $ waveIndex_ * (fromIntegral numWaves)) (numWaves - 1)
  in if V.null table 
      then 0
      else (table ! waveN) ! i