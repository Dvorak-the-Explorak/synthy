{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Voices where

import Control.Monad.State
import Control.Lens

import General (sampleRate, Pulse, Seconds)
import MidiStuff (NoteNumber(..), hzFromNoteNumber)
import Oscillators
import Filters 
import Envelopes 
import Helpers (overState, joinStatesWith)

import Debug.Trace



-- #TODO turn these `type` declarations into `data` declarations and lens it up boi
-- #TODO a voice should be allowed to mix multiple oscillators... 
--  constructing a VoicedSynth should take a voice constructor :: Note -> Voice
--  then we could replace Oscillator with [Oscillator]
data Voice = Voice {
  _voiceOsc :: Oscillator, 
  _voiceVenv :: VolEnv,
  _voiceFiltEnv :: VolEnv,
  _voiceFilt :: Filter,
  _voiceFiltEnvCurve :: FiltEnvCurve, 
  _voiceNote :: NoteNumber
}


-- calls the lens for _voiceFiltEnv filtEnv 
--  using typeclasses so multiple different types can have a filtEnv
makeFields ''Voice


-- ========================================================================

-- #TODO Note off needs to move the filterEnv as well


-- #TODO un-hardcode the oscillator type and ADSR values
voiceFromNote :: NoteNumber -> Voice
voiceFromNote noteNum = defaultVoice  & osc . freq .~ (hzFromNoteNumber noteNum)
                                      & note .~ noteNum


-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice -> Voice
releaseVoice = (over venv noteOffEnv) . (over filtEnv noteOffEnv)

restartVoice :: Voice -> Voice
restartVoice = (over venv restartEnv) . (over filtEnv restartEnv)

-- #TODO default voice could be better thought through
defaultVoice :: Voice
defaultVoice = Voice {
    _voiceOsc = Oscillator {
        _wave=sawTone, _phase=0.0, _freq=1.0
    },
    _voiceVenv = VolEnv {
        _attackSlope=20, _decaySlope=2, _sustainLevel=0.7, 
        _releaseSlope=2, _currentState=EnvAttack, _volume=0
    },
    _voiceFiltEnv = VolEnv {
        _attackSlope=20, _decaySlope=8, _sustainLevel=0.01, 
        _releaseSlope=1, _currentState=EnvAttack, _volume=0
    },
    _voiceFilt = Filter {_prevOut = 0, _cutoff = 400, _filtFunc = lowPass (1/sampleRate)},
    _voiceFiltEnvCurve = FiltEnvCurve (\v -> 800 + 16000*v),
    _voiceNote = 0
}


stepVoice :: Seconds -> State Voice Pulse
stepVoice dt = do
  -- run the filter envelope and update the filter frequency
  -- #TODO Combine the Filter properties into one data type
  -- #TODO Turn this section into a stepFilterEnv stateOp
  filterFreqOffset <- overState filtEnv $ stepEnv dt
  (FiltEnvCurve f) <- gets (view filtEnvCurve)
  let filterFreq = f filterFreqOffset
  modify $ set (filt . cutoff) filterFreq

  -- run the oscillator and the volume envelope
  pulse <- overState osc $ stepOsc dt
  vol <- overState venv $ stepEnv dt

  -- run the filter 
  f <- gets (view $ filt.filtFunc)
  output <- overState filt $ (f $ pulse * vol)

  return $ output


-- #TODO runVoice needs to do the same stuff as stepVoice (ie run the filters)
runVoice :: Int -> Seconds -> State Voice [Pulse]
runVoice n dt = joinStatesWith (zipWith (*)) (overState osc $ runOsc n dt) (overState venv $ runEnv n dt)

