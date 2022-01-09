{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
  #-}

module Synths where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Control.Lens
import Debug.Trace

import Codec.Midi

import General (Seconds, Pulse, sampleRate, Hz)
import Voices (Voice(..), initialiseVoice, defaultVoice, 
          stepVoices, noteOnVoicesWith, noteOffVoices, releaseVoice,
          note, voiceFinished)
import Filters
import Helpers ((.@), stateMap, mapWhere, iterateState, iterateStateUntil)
import MidiStuff (NoteNumber)
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators
import Steppable
import Parameterised



-- FullSynth represents one polyphonic instrument (homogenous voice types)
-- FullSynth is just a [Voice] with a global filter, modulated by LFO

-- type parameter s is the type of oscillator...
data FullSynth s = FullSynth {
  _fullSynthVoices :: [Voice s (Filter FreqParam)], 
  _fullSynthFilt :: Filter Float,
  _fullSynthLfo :: SimpleOsc,
  _fullSynthLfoStrength :: Float,
  _fullSynthVoiceTemplate :: Voice s (Filter FreqParam)
}

makeFields ''FullSynth

instance Steppable Seconds Pulse s => Steppable Seconds Pulse (FullSynth s) where
  step dt  = do
    -- step the [Voice]
    -- pulse <- overState voices $ stepVoices dt
    pulse <- stepVoices dt .@ voices

    -- run the LFO
    moduland <- step dt .@ lfo
    strength <- use lfoStrength

    -- modulate the filter cutoff with the LFO
    filt.param += strength*moduland

    -- -- modulate wavetable indices
    -- -- modify $ over voices $ map (osc.waveIndex .~ (moduland+1)/2)
    -- -- voices %= map (osc.waveIndex .~ (moduland+1)/2)
    -- voices.each.osc.waveIndex .= (moduland+1)/2

    -- -- run the filter to get the output
    output <- step pulse .@ filt

    -- unmodulate the filter cutoff
    filt.param -= strength*moduland

    -- give some headroom 
    return $ 0.1*output

-- ===================================================================================


-- Kill any remaining notes, wait for them to ring out
runFullSynthANiente :: Steppable Seconds Pulse s => Seconds -> State (FullSynth s) [Pulse]
runFullSynthANiente dt = do
  noteOffAllFullSynth
  iterateStateUntil (uses voices null) (step dt)

runFullSynthSteps :: Steppable Seconds Pulse s => Int -> Seconds -> State (FullSynth s) [Pulse]
-- runFullSynthSteps 0 dt = return []
-- runFullSynthSteps n dt = do
--   pulse <- stepFullSynth dt
--   pulses <- runFullSynthSteps (n-1) dt
--   return (pulse:pulses)
runFullSynthSteps n dt = iterateState n (step dt)

-- Chunks the timestep into at most 1 second long chunks
--    this helps when voices end early,
--    as they're only culled once per call to runSynthSteps
--    and leaving them in the EnvDone state will waste time calculating zeros
-- #TODO could the EnvDone state be signalled somehow to automate the culling?
runFullSynth :: Steppable Seconds Pulse s => Seconds -> State (FullSynth s) [Pulse]
runFullSynth dt | dt < (1.0/sampleRate) = return []
                | dt > 1.0 = do 
                    firstSec <- runFullSynthSteps (floor sampleRate) (1.0/sampleRate)
                    remainder <- runFullSynth (dt - 1.0)
                    return $ firstSec ++ remainder
                | otherwise = let n = floor $ dt*sampleRate
                          in runFullSynthSteps n (1.0/sampleRate)



noteOnFullSynth :: (Steppable Seconds Pulse s, FreqField s) => 
                    NoteNumber -> State (FullSynth s) ()
noteOnFullSynth note = do
  newVoice <- use voiceTemplate
  noteOnVoicesWith (initialiseVoice newVoice) note .@ voices

noteOffFullSynth :: NoteNumber -> State (FullSynth s) ()
noteOffFullSynth note = noteOffVoices note .@ voices

noteOffAllFullSynth :: State (FullSynth s) ()
noteOffAllFullSynth = voices.each %= releaseVoice


-- ================================================================================


synthesiseMidiTrack :: (Steppable Seconds Pulse s, FreqField s) => 
                      Track Ticks -> State (FullSynth s) [Pulse]
synthesiseMidiTrack [] = runFullSynthANiente (1/sampleRate)
synthesiseMidiTrack ((ticks, message):messages) = do
    output <- runFullSynthSteps ticks (1/sampleRate)
    case message of
      NoteOff ch key vel -> noteOffFullSynth key 
      NoteOn ch key 0 -> noteOffFullSynth key 
      NoteOn ch key vel -> noteOnFullSynth key
      _ -> return ()
    remainder <- synthesiseMidiTrack messages
    return $ output ++ remainder
-- ==============================================================================



defaultSynth :: FullSynth SimpleOsc
defaultSynth = FullSynth {
  _fullSynthVoices = ([]), 
  -- _fullSynthFilt = bandPass (1/sampleRate) & param .~ (220, 880),--param is (low, high)
  -- _fullSynthFilt = centeredBandPass (1/sampleRate) & param .~ (440, 220),--param is (center, width)
  -- _fullSynthFilt = lowPass (1/sampleRate) & param .~ 440,--param is cutoff frequency
  _fullSynthFilt = cubicFilter & param .~ 0.8,--param is clip limit
  -- _fullSynthFilt = hashtagNoFilter (0,0),
  _fullSynthLfo = lfo1s & freq .~ 0.4,
  _fullSynthLfoStrength = 0.2, -- 400 * 10,
  _fullSynthVoiceTemplate = defaultVoice sawOsc
}


