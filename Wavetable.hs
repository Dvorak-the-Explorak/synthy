module Wavetable where

import Data.WAVE

import Oscillators
import General

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
-- -- assumes 2048 samples per wave
-- oscFromWavetable :: WAVE -> Oscillator
-- oscFromWavetable wav = Oscillator {_wave = waveform, _phase=0, freq=0, _waveIndex=0}
--   where
--     waveform = 