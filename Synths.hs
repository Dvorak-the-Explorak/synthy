{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module Synths where

import General (Seconds, Pulse, sampleRate)
import Voices (Voice(..), voiceFromNote, stepVoice, releaseVoice, restartVoice, note, venv)
import Filters (Filter(..), Filter(..), lowPass, highPass, hashtagNoFilter, mapFilter, filtFunc, cutoff)
import Helpers (stateMap, overState, mapWhere)
import MidiStuff (NoteNumber, ToyMidi(..))
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators (Oscillator, zeroOsc, lfo1s, stepOsc, freq)

import Control.Monad.State
import Control.Lens
import Debug.Trace

-- #TODO should maybe be called an instrument / sequencer 
--  VoicedSynth should include LFOs
 -- should VoicedSynth include current time?

type VoicedSynth = ([Voice])

-- FullSynth is just a VoicedSynth with a global filter, modulated by LFO
data FullSynth = FullSynth {
  _fullSynthVoices :: VoicedSynth, 
  _fullSynthFilt :: Filter,
  _fullSynthLfo :: Oscillator,
  _fullSynthLfoStrength :: Float
}

makeFields ''FullSynth

stepSynthVoices :: Seconds -> State VoicedSynth Pulse
stepSynthVoices dt = fmap sum $ stateMap $ stepVoice dt


-- #TODO this isn't actually running, just iterating steps
-- runVoice n dt :: State Voice [Pulse for each sample]
-- stateMap $ runVoice n dt :: State [Voice] [[Pulse foreach sample] foreach voice]
-- firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [[Pulse foreach sample] foreach voice]
-- fmap (map sum . transpose) $ firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [Pulse foreach sample (summed over voices)]
runSynthVoices :: Int -> Seconds -> State VoicedSynth [Pulse]
runSynthVoices 0 dt = return []
runSynthVoices n dt = do
  pulse <- stepSynthVoices dt
  pulses <- runSynthVoices (n-1) dt
  return (pulse:pulses)


cullSynthVoices :: State VoicedSynth ()
cullSynthVoices = let running = views (venv . currentState) (<EnvDone)
                  in modify (filter running)

-- -- #TODO At some point there should be explicit clipping.  Should it be here?
stepSynth :: Seconds -> State VoicedSynth Pulse
stepSynth dt = do
  output <- stepSynthVoices dt
  cullSynthVoices
  return output

-- #TODO handle midi timing with fractional samples
-- Chunks the timestep into at most 1 second long chunks
--    this helps when voices end early,
--    as they're only culled once per call to runSynthSteps
--    and leaving them in the EnvDone state will waste time calculating zeros
-- #TODO could the EnvDone state be signalled somehow to automate the culling?
runSynth :: Seconds -> State VoicedSynth [Pulse]
runSynth dt | dt < (1.0/sampleRate) = return []
               | dt > 1.0 = do 
                  firstSec <- runSynthSteps (floor sampleRate) (1.0/sampleRate)
                  remainder <- runSynth (dt - 1.0)
                  return $ firstSec ++ remainder
               | otherwise = let n = floor $ dt*sampleRate
                          in runSynthSteps n (1.0/sampleRate)

runSynthSteps :: Int -> Seconds -> State VoicedSynth [Pulse]
runSynthSteps n dt = do
  output <- runSynthVoices n dt
  cullSynthVoices
  return output

restartVoices :: NoteNumber -> VoicedSynth -> VoicedSynth
restartVoices noteNum voices = mapWhere ((==noteNum) . (view note)) restartVoice voices

releaseVoices :: NoteNumber -> VoicedSynth -> VoicedSynth
releaseVoices noteNum voices = mapWhere ((==noteNum) . (view note)) releaseVoice voices

-- if there are any voices for that note, set the envelope to EnvAttack state
--  otherwise add a new voice for the note
noteOnSynth :: NoteNumber -> State VoicedSynth ()
noteOnSynth noteNum = modify $ \voices -> 
    if any ((==noteNum) . (view note)) voices
      -- revert envelope to state 1
      then restartVoices noteNum voices
      -- add a new voice for that note
      else (voiceFromNote noteNum):voices

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffSynth :: NoteNumber -> State VoicedSynth ()
noteOffSynth noteNum = modify $ releaseVoices noteNum 


defaultVoicedSynth :: VoicedSynth
defaultVoicedSynth = ([])



-- ===================================================================================

stepFullSynth :: Seconds -> State FullSynth Pulse
stepFullSynth dt  = do
  -- step the VoicedSynth
  pulse <- overState voices $ stepSynth dt
  
  -- run the LFO
  -- #TODO Oh no I don't have dt in this function. Lets get rid of dt and leave it as a global constant?
  moduland <- overState lfo $ stepOsc dt
  strength <- gets (view lfoStrength)

  -- modulate the filter cutoff with the LFO
  modify $ over (filt.cutoff) (\x -> x+(moduland*strength))
  -- get the filter
  _filt <- gets (view $ filt . filtFunc)

  -- run the filter to get the output
  output <- overState filt $ _filt pulse

  -- unmodulate the filter cutoff
  modify $ over (filt.cutoff) (\x -> x-moduland*strength)

  return output

runFullSynthSteps :: Int -> Seconds -> State FullSynth [Pulse]
runFullSynthSteps 0 dt = return []
runFullSynthSteps n dt = do
  pulse <- stepFullSynth dt
  pulses <- runFullSynthSteps (n-1) dt
  return (pulse:pulses)

runFullSynth :: Seconds -> State FullSynth [Pulse]
runFullSynth dt | dt < (1.0/sampleRate) = return []
                | dt > 1.0 = do 
                    firstSec <- runFullSynthSteps (floor sampleRate) (1.0/sampleRate)
                    remainder <- runFullSynth (dt - 1.0)
                    return $ firstSec ++ remainder
                | otherwise = let n = floor $ dt*sampleRate
                          in runFullSynthSteps n (1.0/sampleRate)

noteOnFullSynth :: NoteNumber -> State FullSynth ()
noteOnFullSynth note = overState voices $ noteOnSynth note

noteOffFullSynth :: NoteNumber -> State FullSynth ()
noteOffFullSynth note = overState voices $ noteOffSynth note


-- ================================================================================

synthesiseMidiVoicedSynth :: [ToyMidi] -> State VoicedSynth [Pulse]
synthesiseMidiVoicedSynth [] = return []
synthesiseMidiVoicedSynth ((ToyNoteOn note dt):mids) = do 
  output <- runSynth dt
  noteOnSynth note
  remainder <- synthesiseMidiVoicedSynth mids
  return $ output ++ remainder
synthesiseMidiVoicedSynth ((ToyNoteOff note dt):mids) = do
    output <- runSynth dt
    noteOffSynth note
    remainder <- synthesiseMidiVoicedSynth mids
    return $ output ++ remainder
synthesiseMidiVoicedSynth ((ToyNothing dt):mids) = do
    output <- runSynth dt
    remainder <- synthesiseMidiVoicedSynth mids
    return $ output ++ remainder


synthesiseMidiFullSynth :: [ToyMidi] -> State FullSynth [Pulse]
synthesiseMidiFullSynth [] = return []
synthesiseMidiFullSynth ((ToyNoteOn note dt):mids) = do 
  output <- runFullSynth dt
  noteOnFullSynth note
  remainder <- synthesiseMidiFullSynth mids
  return $ output ++ remainder
synthesiseMidiFullSynth ((ToyNoteOff note dt):mids) = do
    output <- runFullSynth dt
    noteOffFullSynth note
    remainder <- synthesiseMidiFullSynth mids
    return $ output ++ remainder
synthesiseMidiFullSynth ((ToyNothing dt):mids) = do
    output <- runFullSynth dt
    remainder <- synthesiseMidiFullSynth mids
    return $ output ++ remainder


-- ==============================================================================

defaultSynth :: FullSynth
defaultSynth = FullSynth {
  _fullSynthVoices = ([]), 
  _fullSynthFilt = Filter {_prevOut =0, _cutoff = 800, _filtFunc = highPass (1/sampleRate)},
  _fullSynthLfo = lfo1s & freq .~ 4,
  _fullSynthLfoStrength = 400 * 10
}


