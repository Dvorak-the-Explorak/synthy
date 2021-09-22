module Synths where


import General (Seconds, Pulse, sampleRate)
import Voices (Voice(..), voiceFromNote, stepVoice, releaseVoice, restartVoice, note, venv)
import Filters (Filter(..), Filter(..), hashtagNoFilter, mapFilter, filtFunc)
import Helpers (stateMap, overState, mapWhere)
import MidiStuff (NoteNumber, ToyMidi(..))
import Envelopes (VolEnv(..), EnvSegment(..), currentState)

import Control.Monad.State
import Control.Lens

-- #TODO should maybe be called an instrument / sequencer 
--  VoicedSynth should include LFOs
 -- should VoicedSynth include current time?

type VoicedSynth = ([Voice])
-- FullSynth is just a VoicedSynth with a global filter
type FullSynth = (VoicedSynth, Filter)



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


applySynthOp :: State VoicedSynth Pulse -> State FullSynth Pulse
applySynthOp op = do
  pulse <- overState _1 op
  _filt <- gets (view $ _2.filtFunc)
  overState _2 (_filt pulse)

applySynthOps :: State VoicedSynth [Pulse] -> State FullSynth [Pulse]
applySynthOps op = do 
  pulses <- overState _1 op
  _filt <- gets (view $ _2 . filtFunc)
  overState _2 (mapFilter _filt pulses) 

applySynthMod :: State VoicedSynth () -> State FullSynth ()
applySynthMod = overState _1

stepFullSynth :: Seconds -> State FullSynth Pulse
stepFullSynth dt = applySynthOp (stepSynth dt)

runFullSynthSteps :: Int -> Seconds -> State FullSynth [Pulse]
runFullSynthSteps n dt = applySynthOps $ runSynthSteps n dt

runFullSynth :: Seconds -> State FullSynth [Pulse]
runFullSynth dt = applySynthOps $ runSynth dt

noteOnFullSynth :: NoteNumber -> State FullSynth ()
noteOnFullSynth note = applySynthMod $ noteOnSynth note

noteOffFullSynth :: NoteNumber -> State FullSynth ()
noteOffFullSynth note = applySynthMod $ noteOffSynth note

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
defaultSynth = (([]), Filter {_prevOut =0, _cutoff = 800, _filtFunc = hashtagNoFilter})
