{-#  LANGUAGE BangPatterns 
#-}

module Wavetable where

import Data.WAVE
import qualified Data.Vector as V

import General
import Helpers

import Debug.Trace

type Wavetable = V.Vector (V.Vector Pulse)

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


-- #TODO make wavetable just a function, so that Oscillators.hs doesn't need to know if it's vector or list
loadWavetable :: Int -> WAVE -> Wavetable
loadWavetable n wav = let groups = group n $ samplesFromWave wav
                          vectors = map V.fromList $ filter ((==n) . length) groups
                      in V.fromList vectors