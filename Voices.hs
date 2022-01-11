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
To be able to easily add filters / modulators to a voice

A function:
makeVoice :: Source s => s -> VolEnv -> 

-}


-- #TODO want `IsVoice v` to imply `Source v`
class IsVoice v where
  restart :: v -> v
  release :: v -> v
  finished :: v -> Bool
  -- set the internal state of this voice to the appropriate values for a given note
  --  (eg. set oscillator frequencies)
  initialise :: NoteNumber -> v -> v


data Voice s f = Voice {
  _voiceSource :: s, 
  _voiceVenv :: VolEnv,
  _voiceFiltEnv :: VolEnv,
  _voiceFilt :: f,
  -- How to change the filter according to the output of the filter envelope
  _voiceFiltModulate :: (Volume -> f -> f)
}


-- calls the lens for _voiceFiltEnv filtEnv 
--  using typeclasses so multiple different types can have a filtEnv
makeFields ''Voice

instance FreqField s => IsVoice (Voice s f) where
  restart = restartVoice
  release = releaseVoice
  finished = voiceFinished
  initialise = initialiseVoice

-- Source == Steppable Seconds Pulse
instance (Source s, Transformer f) 
        => Steppable Seconds Pulse (Voice s f) where
  step dt =  do
    -- run the filter envelope and update the filter frequency
    stepFilterEnv dt

    -- run the oscillator and the volume envelope
    pulse <-  step dt .@ source
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


initialiseVoice :: FreqField s => NoteNumber -> Voice s f -> Voice s f
initialiseVoice noteNum v = v & source . freq .~ hzFromNoteNumber noteNum


voiceFinished :: Voice s f -> Bool
voiceFinished v = (v ^. venv . currentState) == EnvDone

-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice s f -> Voice s f
releaseVoice = (venv %~ noteOffEnv) . (filtEnv %~ noteOffEnv)

restartVoice :: Voice s f -> Voice s f
restartVoice = (venv %~ restartEnv) . (filtEnv %~ restartEnv)

-- ===============================================================================



-- #TODO default voice could be better thought through
-- #TODO make a function/functions to change the envelope of a voice
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
    _voiceFiltModulate = (\ v f -> f & freq .~ 800 + 16000*v)
}
