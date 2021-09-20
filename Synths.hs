module Synths where


import Data.Fixed
import General


type Synth = Hz -> Seconds -> [Pulse]
type Waveform = Seconds -> Pulse
type SynthGenerator = Waveform -> Synth


pureSynth :: Synth
pureSynth = makeSynth pureTone
sawSynth :: Synth
sawSynth = makeSynth sawTone
squareSynth :: Synth
squareSynth = makeSynth squareTone


-- -- hmm
-- whiteNoise :: Synth
-- whtieNoise = makeSynth 

pureTone :: Waveform
pureTone = (sin . (*) (2*pi))

sawTone :: Waveform
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Waveform
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)

-- synced to start of note only, no globally timed LFO
makePWMSynth :: Waveform -> SynthGenerator
makePWMSynth lfo = makeSynth . mult modulator
    where
        mult f g x = (f x) * (g x)
        modulator = (\x -> if (x `mod'` 1.0 < lfo x) then 1.0 else 0.0)

-- take frequency, duration 
-- return samples
makeSynth :: SynthGenerator
makeSynth baseWave  = \toneFreq duration -> map (\t ->  volume * baseWave( t * toneFreq/sampleRate)) [0.0 .. sampleRate*duration]
        where 
            volume = 0.5