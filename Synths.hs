module Synths where


import Data.Fixed
import General


type Synth = Hz -> Seconds -> [Pulse]
type Oscillator = Seconds -> Pulse
type SynthGenerator = Oscillator -> Synth


defaultSynth :: Synth
-- synth = makePulsedSynth 0.8 pureTone
defaultSynth = sawSynth 
-- synth = pureSynth

pureSynth :: Synth
pureSynth = makeSynth pureTone
sawSynth :: Synth
sawSynth = makeSynth sawTone
squareSynth :: Synth
squareSynth = makeSynth squareTone


-- -- hmm
-- whiteNoise :: Synth
-- whtieNoise = makeSynth 

pureTone :: Oscillator
pureTone = (sin . (*) (2*pi))

sawTone :: Oscillator
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Oscillator
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)

-- synced to start of note only, no globally timed LFO
makePWMSynth :: Oscillator -> SynthGenerator
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