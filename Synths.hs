{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module Synths where

import General (Seconds, Pulse, sampleRate, Hz)
import Voices (Voice(..), initialiseVoice, defaultVoice, releaseVoice, restartVoice, note, venv, osc)
import Filters
import Helpers ((.@), stateMap, mapWhere, iterateState, iterateStateUntil)
import MidiStuff (NoteNumber)
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators (Oscillator, zeroOsc, lfo1s, freq, waveIndex)
import Steppable

import Control.Monad.State
import Control.Lens
import Debug.Trace

import Codec.Midi

-- FullSynth is just a [Voice] with a global filter, modulated by LFO
data FullSynth = FullSynth {
  _fullSynthVoices :: [Voice Hz], 
  _fullSynthFilt :: Filter Hz,
  _fullSynthLfo :: Oscillator,
  _fullSynthLfoStrength :: Float,
  _fullSynthVoiceTemplate :: Voice Hz
}

makeFields ''FullSynth

instance Steppable Pulse FullSynth where
  step dt  = do
    -- step the [Voice]
    -- pulse <- overState voices $ stepVoices dt
    pulse <- stepVoices dt .@ voices

    -- run the LFO
    moduland <- step dt .@ lfo
    strength <- use lfoStrength

    -- modulate the filter cutoff with the LFO
    filt.param += strength*moduland

    -- modulate wavetable indices
    -- modify $ over voices $ map (osc.waveIndex .~ (moduland+1)/2)
    -- voices %= map (osc.waveIndex .~ (moduland+1)/2)
    voices.each.osc.waveIndex .= (moduland+1)/2

    -- -- run the filter to get the output
    output <- runFilter pulse .@ filt

    -- unmodulate the filter cutoff
    filt.param -= strength*moduland

    -- give some headroom 
    return $ 0.1*output

stepVoices :: Seconds -> State [Voice a] Pulse
stepVoices dt = do
  output <- fmap sum $ stateMap $ step dt
  cullVoices
  return output


-- #TODO this isn't actually "running", just iterating steps
runVoicesSteps :: Int -> Seconds -> State [Voice a] [Pulse]
runVoicesSteps n dt = iterateState n (stepVoices dt)


cullVoices :: State [Voice a] ()
cullVoices = modify (filter running)
  where running = views (venv . currentState) (<EnvDone)

restartVoices :: NoteNumber -> [Voice a] -> [Voice a]
restartVoices noteNum voices = mapWhere ((==noteNum) . (view note)) restartVoice voices

releaseVoices :: NoteNumber -> [Voice a] -> [Voice a]
releaseVoices noteNum voices = mapWhere ((==noteNum) . (view note)) releaseVoice voices

-- if there are any voices for that note, set the envelope to EnvAttack state
--  otherwise add a new voice for the note
noteOnVoicesWith :: (NoteNumber -> Voice a) ->  NoteNumber -> State [Voice a] ()
noteOnVoicesWith makeVoice noteNum = modify $ \voices -> 
    if any ((==noteNum) . (view note)) voices
      -- revert envelope to state 1
      then restartVoices noteNum voices
      -- add a new voice for that note
      else (makeVoice noteNum ):voices

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffVoices :: NoteNumber -> State [Voice a] ()
noteOffVoices noteNum = modify $ releaseVoices noteNum 

-- ===================================================================================



runFullSynthANiente :: Seconds -> State FullSynth [Pulse]
runFullSynthANiente dt = do
  noteOffAllFullSynth
  iterateStateUntil (uses voices null) (step dt)

runFullSynthSteps :: Int -> Seconds -> State FullSynth [Pulse]
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
runFullSynth :: Seconds -> State FullSynth [Pulse]
runFullSynth dt | dt < (1.0/sampleRate) = return []
                | dt > 1.0 = do 
                    firstSec <- runFullSynthSteps (floor sampleRate) (1.0/sampleRate)
                    remainder <- runFullSynth (dt - 1.0)
                    return $ firstSec ++ remainder
                | otherwise = let n = floor $ dt*sampleRate
                          in runFullSynthSteps n (1.0/sampleRate)



noteOnFullSynth :: NoteNumber -> State FullSynth ()
noteOnFullSynth note = do
  newVoice <- use voiceTemplate
  noteOnVoicesWith (initialiseVoice newVoice) note .@ voices

noteOffFullSynth :: NoteNumber -> State FullSynth ()
noteOffFullSynth note = noteOffVoices note .@ voices

noteOffAllFullSynth :: State FullSynth ()
noteOffAllFullSynth = voices.each %= releaseVoice


-- ================================================================================


synthesiseMidiTrack :: Track Ticks -> State FullSynth [Pulse]
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



defaultSynth :: FullSynth
defaultSynth = FullSynth {
  _fullSynthVoices = ([]), 
  -- _fullSynthFilt = bandPass (1/sampleRate) & param .~ (220, 880),--param is (low, high)
  -- _fullSynthFilt = centeredBandPass (1/sampleRate) & param .~ (440, 220),--param is (center, width)
  -- _fullSynthFilt = lowPass (1/sampleRate) & param .~ 440,--param is cutoff frequency
  _fullSynthFilt = cubicFilter & param .~ 0.8,--param is clip limit
  -- _fullSynthFilt = hashtagNoFilter (0,0),
  _fullSynthLfo = lfo1s & freq .~ 0.4,
  _fullSynthLfoStrength = 0.2, -- 400 * 10,
  _fullSynthVoiceTemplate = defaultVoice
}


