{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , ExistentialQuantification
           , ScopedTypeVariables
  #-}

module Voices where

-- A voice is an enveloped sound source, with a NoteNumber attached.  





import Control.Monad.State
import Control.Lens

import General (sampleRate, Pulse, Seconds, Volume, Hz)
import MidiStuff (NoteNumber(..), hzFromNoteNumber)
import Oscillators
import Filters 
import Envelopes 
import Steppable
import Parameterised
import Helpers ((.@), iterateState, stateMap)


import Debug.Trace

{-   WANT:
To be able to use different oscillators*, and use the parameters they expose,
  while keeping the type-level safety (and without much overhead)

  
* oscillators with different parameters (just freq, freq & waveIndex)

-}

class IsVoice v where
  restart :: v -> v
  release :: v -> v
  finished :: v -> Bool
  maybeRestart :: NoteNumber -> v -> Maybe v
  --        | which note to turn on
  --        |             | voice template to use
  noteOn :: NoteNumber -> v -> State [v] ()
  --        | which note to turn off
  noteOff :: NoteNumber -> State [v] ()


data Voice s f = Voice {
  _voiceSource :: s, 
  _voiceVenv :: VolEnv,
  _voiceFiltEnv :: VolEnv,
  _voiceFilt :: f,
  _voiceFiltModulate :: (Volume -> f -> f), 
  _voiceNote :: NoteNumber
}


-- calls the lens for _voiceFiltEnv filtEnv 
--  using typeclasses so multiple different types can have a filtEnv
makeFields ''Voice

instance FreqField s => IsVoice (Voice s f) where
  restart = restartVoice
  release = releaseVoice
  finished = voiceFinished
  maybeRestart = maybeRestartVoice
  noteOn note template = noteOnVoicesWith (initialiseVoice template) note
  noteOff note = noteOffVoices note

-- Source == Steppable Seconds Pulse
instance (Source s, Transformer f) 
        => Steppable Seconds Pulse (Voice s f) where
  step dt =  do
    -- run the filter envelope and update the filter frequency
    stepFilterEnv dt

    -- run the oscillator and the volume envelope
    pulse :: Pulse <-  step dt .@ source
    vol <- step dt .@ venv

    -- run the filter
    output <- step (pulse*vol) .@ filt

    return $ output

  -- run the filter envelope and update the filter frequency
stepFilterEnv :: Seconds -> State (Voice s f) ()
stepFilterEnv dt = do
  filterFreqOffset <- step dt .@ filtEnv
  f <- use filtModulate
  filt %= f filterFreqOffset


instance FreqField s => FreqField (Voice s f) where
  freq = source . freq

instance WaveIndexField s => WaveIndexField (Voice s f) where
  waveIndex = source . waveIndex

-- =======================================================================

initialiseVoice :: FreqField s => Voice s f -> NoteNumber -> Voice s f
initialiseVoice v noteNum = v & source . freq .~ hzFromNoteNumber noteNum
                              & note .~ noteNum


voiceFinished :: Voice s f -> Bool
voiceFinished v = (v ^. venv . currentState) == EnvDone

-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice s f -> Voice s f
releaseVoice = (venv %~ noteOffEnv) . (filtEnv %~ noteOffEnv)

restartVoice :: Voice s f -> Voice s f
restartVoice = (venv %~ restartEnv) . (filtEnv %~ restartEnv)

maybeRestartVoice :: NoteNumber -> Voice s f -> Maybe (Voice s f)
maybeRestartVoice noteNum v = 
  if (v ^. note) == noteNum
    then Just $ restartVoice v
    else Nothing



onMatchingVoice :: NoteNumber -> (Voice s f -> Voice s f) -> Voice s f -> Voice s f
onMatchingVoice noteNum f v = 
  if (v ^. note) == noteNum
    then f v
    else v



restartMatchingVoice :: NoteNumber -> Voice s f -> Voice s f
restartMatchingVoice note = onMatchingVoice note restartVoice

releaseMatchingVoice :: NoteNumber -> Voice s f -> Voice s f
releaseMatchingVoice note = onMatchingVoice note releaseVoice


-- ==================================================================================

stepVoices :: (Source s, Transformer f) 
        => Seconds -> State [Voice s f] Pulse
stepVoices dt = do
  output <- fmap sum $ stateMap $ step dt
  cullVoices
  return output

cullVoices :: State [Voice s f] ()
cullVoices = modify (filter running)
  where running = not . voiceFinished

cullFinished :: IsVoice v => State [v] ()
cullFinished = modify (filter running)
  where running = not . finished

-- send any voice with given noteNumber back to the start of its envelope
restartVoices :: NoteNumber -> [Voice s f] -> [Voice s f]
restartVoices noteNum voices = map (restartMatchingVoice noteNum) voices

-- noteOff any voice with given noteNumber (send them to the release part of envelope)
releaseVoices :: NoteNumber -> [Voice s f] -> [Voice s f]
releaseVoices noteNum voices = map (releaseMatchingVoice noteNum) voices

-- Traverse the list of voices with a maybeRestartVoice function, 
--   if the result is Nothing, create a new one
noteOnVoicesWith :: (NoteNumber -> Voice s f) ->  NoteNumber -> State [Voice s f] ()
noteOnVoicesWith makeVoice noteNum = modify $ \voices -> 
  let 
    go [] = [makeVoice noteNum]
    go (v:vs) = case maybeRestartVoice noteNum v of
                  Nothing -> v : go vs
                  Just v' -> v' : vs
  in go voices

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffVoices :: NoteNumber -> State [Voice s f] ()
noteOffVoices noteNum = modify $ releaseVoices noteNum 


-- ===============================================================================



-- #TODO default voice could be better thought through
defaultVoice :: s -> Voice s (Filter FreqParam)
defaultVoice source = Voice {
    _voiceSource = source,
    _voiceVenv = VolEnv {
        _attackSlope=20, _decaySlope=2, _sustainLevel=0.7, 
        _releaseSlope=2, _currentState=EnvAttack, _volume=0
    },
    _voiceFiltEnv = VolEnv {
        _attackSlope=20, _decaySlope=8, _sustainLevel=0.01, 
        _releaseSlope=1, _currentState=EnvAttack, _volume=0
    },
    _voiceFilt = (lowPass (1/sampleRate)) & freq .~ 400,
    _voiceFiltModulate = (\ v f -> f & freq .~ 800 + 16000*v),
    _voiceNote = 0
}
