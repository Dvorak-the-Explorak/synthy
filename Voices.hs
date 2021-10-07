{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module Voices where

import Control.Monad.State
import Control.Lens

import General (sampleRate, Pulse, Seconds, Volume, Hz)
import MidiStuff (NoteNumber(..), hzFromNoteNumber)
import Oscillators
import Filters 
import Envelopes 
import Helpers (overState, joinStatesWith)

import Debug.Trace



-- #TODO a voice should be allowed to mix multiple oscillators... 
--    we could replace Oscillator with [Oscillator]
data Voice a = Voice {
  _voiceOsc :: Oscillator, 
  _voiceVenv :: VolEnv,
  _voiceFiltEnv :: VolEnv,
  _voiceFilt :: Filter a,
  _voiceFiltEnvCurve :: (Volume -> a), 
  _voiceNote :: NoteNumber
}


-- calls the lens for _voiceFiltEnv filtEnv 
--  using typeclasses so multiple different types can have a filtEnv
makeFields ''Voice


-- =======================================================================

initialiseVoice :: Voice a -> NoteNumber -> Voice a
initialiseVoice v noteNum = v & osc . freq .~ (hzFromNoteNumber noteNum)
                              & note .~ noteNum

-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice a -> Voice a
releaseVoice = (over venv noteOffEnv) . (over filtEnv noteOffEnv)

restartVoice :: Voice a -> Voice a
restartVoice = (over venv restartEnv) . (over filtEnv restartEnv)

-- #TODO default voice could be better thought through
defaultVoice :: Voice Hz
defaultVoice = Voice {
    _voiceOsc = sawOsc,
    _voiceVenv = VolEnv {
        _attackSlope=20, _decaySlope=2, _sustainLevel=0.7, 
        _releaseSlope=2, _currentState=EnvAttack, _volume=0
    },
    _voiceFiltEnv = VolEnv {
        _attackSlope=20, _decaySlope=8, _sustainLevel=0.01, 
        _releaseSlope=1, _currentState=EnvAttack, _volume=0
    },
    _voiceFilt = (lowPass (1/sampleRate)) & param .~ 400,
    _voiceFiltEnvCurve = (\v -> 800 + 16000*v),
    _voiceNote = 0
}

  -- run the filter envelope and update the filter frequency
stepFilterEnv :: Seconds -> State (Voice a) ()
stepFilterEnv dt = do
  filterFreqOffset <- overState filtEnv $ stepEnv dt
  f <- use filtEnvCurve
  filt.param .= f filterFreqOffset


stepVoice :: Seconds -> State (Voice a) Pulse
stepVoice dt = do

  -- run the filter envelope and update the filter frequency
  stepFilterEnv dt

  -- run the oscillator and the volume envelope
  pulse <- overState osc $ stepOsc dt
  vol <- overState venv $ stepEnv dt

  -- run the filter
  output <- overState filt $ runFilter (pulse*vol)

  return $ output


-- #TODO runVoice needs to do the same stuff as stepVoice (ie run the filters)
runVoice :: Int -> Seconds -> State (Voice a) [Pulse]
runVoice n dt = joinStatesWith (zipWith (*)) (overState osc $ runOsc n dt) (overState venv $ runEnv n dt)

