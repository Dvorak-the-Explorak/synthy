{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , ScopedTypeVariables
  #-}

module Synths where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Control.Lens
import Debug.Trace
import Data.List (sum)

import Data.Map (Map)
import qualified Data.Map as Map

import Codec.Midi

import General (Seconds, Pulse, sampleRate, Hz)
import Voices
-- import Voices (Voice(..), IsVoice, initialiseVoice, defaultVoice, 
--           stepVoices, noteOnVoicesWith, noteOffVoices, releaseVoice,
--           note, voiceFinished, cullVoices)
import Filters
import Helpers ((.@), stateMap, injectState, mapWhere, iterateState, iterateStateUntil)
import MidiStuff (NoteNumber)
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators
import Steppable
import Parameterised



-- FullSynth represents one polyphonic instrument (homogenous voice types)
-- FullSynth is just a [Voice] with a global filter, modulated by LFO

-- type parameter s is the type of oscillator...
data FullSynth s = FullSynth {
  -- _fullSynthVoices :: Map.Map NoteNumber s, 
  _fullSynthVoices :: [s], 
  _fullSynthFilt :: Filter Float,
  _fullSynthLfo :: SimpleOsc,
  _fullSynthLfoStrength :: Float,
  _fullSynthVoiceTemplate :: s
}

makeFields ''FullSynth

-- Source == Steppable Seconds Pulse
instance (Source s, IsVoice s) => Steppable Seconds Pulse (FullSynth s) where
  step dt  = do
    -- step the [Voice]
    pulses <- stateMap (step dt) .@ voices
    -- pulses <- traverse (step dt) .@ voices
    let pulse = sum pulses
    cullFinished .@ voices

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
runFullSynthANiente :: (Source s, IsVoice s) => Seconds -> State (FullSynth s) [Pulse]
runFullSynthANiente dt = do
  noteOffAllFullSynth
  iterateStateUntil (uses voices null) (step dt)

runFullSynthSteps :: (Source s, IsVoice s) => Int -> Seconds -> State (FullSynth s) [Pulse]
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
runFullSynth :: (Source s, IsVoice s) => Seconds -> State (FullSynth s) [Pulse]
runFullSynth dt | dt < (1.0/sampleRate) = return []
                | dt > 1.0 = do 
                    firstSec <- runFullSynthSteps (floor sampleRate) (1.0/sampleRate)
                    remainder <- runFullSynth (dt - 1.0)
                    return $ firstSec ++ remainder
                | otherwise = let n = floor $ dt*sampleRate
                          in runFullSynthSteps n (1.0/sampleRate)



noteOnFullSynth :: (Source s, FreqField s, IsVoice s) => 
                    NoteNumber -> State (FullSynth s) ()
noteOnFullSynth note = do
  newVoice <- use voiceTemplate
  -- noteOnVoicesWith (initialiseVoice newVoice) note .@ voices
  noteOn note newVoice .@ voices
  -- noteOnVoicesWith (initialiseVoice newVoice) note .@ voices

noteOffFullSynth :: IsVoice s => NoteNumber -> State (FullSynth s) ()
noteOffFullSynth note = noteOff note .@ voices

noteOffAllFullSynth :: IsVoice s => State (FullSynth s) ()
noteOffAllFullSynth = voices.each %= release


-- ================================================================================


synthesiseMidiTrack :: (Source s, FreqField s, IsVoice s) => 
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



defaultSynth :: FullSynth (Voice SimpleOsc (Filter FreqParam))
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


