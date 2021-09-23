{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module Synths where

import General (Seconds, Pulse, sampleRate)
import Voices (Voice(..), defaultMakeVoice, stepVoice, releaseVoice, restartVoice, note, venv)
import Filters (Filter(..), Filter(..), lowPass, highPass, hashtagNoFilter, mapFilter, filtFunc, cutoff)
import Helpers (stateMap, overState, mapWhere)
import MidiStuff (NoteNumber)
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators (Oscillator, zeroOsc, lfo1s, stepOsc, freq)

import Control.Monad.State
import Control.Lens
import Debug.Trace

import Codec.Midi

-- type VoicedSynth = ([Voice])

-- FullSynth is just a [Voice] with a global filter, modulated by LFO
data FullSynth = FullSynth {
  _fullSynthVoices :: [Voice], 
  _fullSynthFilt :: Filter,
  _fullSynthLfo :: Oscillator,
  _fullSynthLfoStrength :: Float,
  _fullSynthMakeVoice :: (NoteNumber -> Voice)
}

makeFields ''FullSynth

stepVoices :: Seconds -> State [Voice] Pulse
stepVoices dt = do
  output <- fmap sum $ stateMap $ stepVoice dt
  cullVoices
  return output


-- #TODO this isn't actually running, just iterating steps
-- runVoice n dt :: State Voice [Pulse for each sample]
-- stateMap $ runVoice n dt :: State [Voice] [[Pulse foreach sample] foreach voice]
-- firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [[Pulse foreach sample] foreach voice]
-- fmap (map sum . transpose) $ firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [Pulse foreach sample (summed over voices)]
runVoicesSteps :: Int -> Seconds -> State [Voice] [Pulse]
runVoicesSteps 0 dt = return []
runVoicesSteps n dt = do
  pulse <- stepVoices dt
  pulses <- runVoicesSteps (n-1) dt
  return (pulse:pulses)


cullVoices :: State [Voice] ()
cullVoices = let running = views (venv . currentState) (<EnvDone)
                  in modify (filter running)

restartVoices :: NoteNumber -> [Voice] -> [Voice]
restartVoices noteNum voices = mapWhere ((==noteNum) . (view note)) restartVoice voices

releaseVoices :: NoteNumber -> [Voice] -> [Voice]
releaseVoices noteNum voices = mapWhere ((==noteNum) . (view note)) releaseVoice voices

-- if there are any voices for that note, set the envelope to EnvAttack state
--  otherwise add a new voice for the note
noteOnVoicesWith :: (NoteNumber -> Voice) ->  NoteNumber -> State [Voice] ()
noteOnVoicesWith makeVoice noteNum = modify $ \voices -> 
    if any ((==noteNum) . (view note)) voices
      -- revert envelope to state 1
      then restartVoices noteNum voices
      -- add a new voice for that note
      else (makeVoice noteNum):voices

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffVoices :: NoteNumber -> State [Voice] ()
noteOffVoices noteNum = modify $ releaseVoices noteNum 


-- ===================================================================================

stepFullSynth :: Seconds -> State FullSynth Pulse
stepFullSynth dt  = do
  -- step the [Voice]
  pulse <- overState voices $ stepVoices dt
  
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

-- #TODO handle midi timing with fractional samples
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
  make <- gets (view $ makeVoice)
  overState voices $ noteOnVoicesWith make note

noteOffFullSynth :: NoteNumber -> State FullSynth ()
noteOffFullSynth note = overState voices $ noteOffVoices note


-- ================================================================================


synthesiseMidiTrack :: Track Ticks -> State FullSynth [Pulse]
synthesiseMidiTrack [] = return []
synthesiseMidiTrack ((ticks, NoteOn ch key vel):messages) = 
  if vel == 0 
    then synthesiseMidiTrack ((ticks, NoteOff ch key vel):messages)
    else do 
      output <- runFullSynthSteps ticks (1/sampleRate)
      noteOnFullSynth key
      voices <- gets (view voices)
      remainder <- synthesiseMidiTrack messages
      return $ output ++ remainder
synthesiseMidiTrack ((ticks, NoteOff {key=key}):messages) = do
    output <- runFullSynthSteps ticks (1/sampleRate)
    noteOffFullSynth key 
    remainder <- synthesiseMidiTrack messages
    return $ output ++ remainder
synthesiseMidiTrack ((ticks, message):messages) = do
    output <- runFullSynthSteps ticks (1/sampleRate)
    remainder <- synthesiseMidiTrack messages
    return $ output ++ remainder
-- ==============================================================================

defaultSynth :: FullSynth
defaultSynth = FullSynth {
  _fullSynthVoices = ([]), 
  _fullSynthFilt = Filter {_prevOut =0, _cutoff = 800, _filtFunc = highPass (1/sampleRate)},
  _fullSynthLfo = lfo1s & freq .~ 4,
  _fullSynthLfoStrength = 400 * 10,
  _fullSynthMakeVoice = defaultMakeVoice
}


