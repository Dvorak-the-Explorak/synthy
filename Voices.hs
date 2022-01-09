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

-- A voice is a single active oscillator in a synth, attached to one NoteNumber. 
--  it has an oscillator, a volume envelope, a note number, and its own filter.





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





-- #TODO a voice should be allowed to mix multiple oscillators... 
--    we could replace Oscillator with [Oscillator]

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


instance (Steppable Seconds Pulse s, Steppable Pulse Pulse b) 
        => Steppable Seconds Pulse (Voice s b) where
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
stepFilterEnv :: Seconds -> State (Voice s b) ()
stepFilterEnv dt = do
  filterFreqOffset <- step dt .@ filtEnv
  f <- use filtModulate
  filt %= f filterFreqOffset


instance FreqField s => FreqField (Voice s f) where
  freq = source . freq

instance WaveIndexField s => WaveIndexField (Voice s f) where
  waveIndex = source . waveIndex

-- =======================================================================

initialiseVoice :: FreqField s => Voice s b -> NoteNumber -> Voice s b
initialiseVoice v noteNum = v & source . freq .~ hzFromNoteNumber noteNum
                              & note .~ noteNum


voiceFinished :: Voice s b -> Bool
voiceFinished v = (v ^. venv . currentState) == EnvDone

-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice s b -> Voice s b
releaseVoice = (venv %~ noteOffEnv) . (filtEnv %~ noteOffEnv)

restartVoice :: Voice s b -> Voice s b
restartVoice = (venv %~ restartEnv) . (filtEnv %~ restartEnv)

maybeRestartVoice :: NoteNumber -> Voice s b -> Maybe (Voice s b)
maybeRestartVoice noteNum v = 
  if (v ^. note) == noteNum
    then Just $ restartVoice v
    else Nothing



onMatchingVoice :: NoteNumber -> (Voice s b -> Voice s b) -> Voice s b -> Voice s b
onMatchingVoice noteNum f v = 
  if (v ^. note) == noteNum
    then f v
    else v



restartMatchingVoice :: NoteNumber -> Voice s b -> Voice s b
restartMatchingVoice note = onMatchingVoice note restartVoice

releaseMatchingVoice :: NoteNumber -> Voice s b -> Voice s b
releaseMatchingVoice note = onMatchingVoice note releaseVoice


-- ==================================================================================

stepVoices :: (Steppable Seconds Pulse s, Steppable Pulse Pulse b) 
        => Seconds -> State [Voice s b] Pulse
stepVoices dt = do
  output <- fmap sum $ stateMap $ step dt
  cullVoices
  return output

cullVoices :: State [Voice s b] ()
cullVoices = modify (filter running)
  where running = not . voiceFinished

-- send any voice with given noteNumber back to the start of its envelope
restartVoices :: NoteNumber -> [Voice s b] -> [Voice s b]
restartVoices noteNum voices = map (restartMatchingVoice noteNum) voices

-- noteOff any voice with given noteNumber (send them to the release part of envelope)
releaseVoices :: NoteNumber -> [Voice s b] -> [Voice s b]
releaseVoices noteNum voices = map (releaseMatchingVoice noteNum) voices

-- Traverse the list of voices with a maybeRestartVoice function, 
--   if the result is Nothing, create a new one
noteOnVoicesWith :: (NoteNumber -> Voice s b) ->  NoteNumber -> State [Voice s b] ()
noteOnVoicesWith makeVoice noteNum = modify $ \voices -> 
  let 
    go [] = [makeVoice noteNum]
    go (v:vs) = case maybeRestartVoice noteNum v of
                  Nothing -> v : go vs
                  Just v' -> v' : vs
  in go voices

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffVoices :: NoteNumber -> State [Voice s b] ()
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
