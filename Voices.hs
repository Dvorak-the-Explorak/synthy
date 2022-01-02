{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , RankNTypes
           , ExistentialQuantification
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
import Helpers ((.@), iterateState)


import Debug.Trace



-- #TODO a voice should be allowed to mix multiple oscillators... 
--    we could replace Oscillator with [Oscillator]

data Voice a b = Voice {
  _voiceOsc :: Oscillator a, 
  _voiceVenv :: VolEnv,
  _voiceFiltEnv :: VolEnv,
  _voiceFilt :: Filter b,
  _voiceFiltEnvCurve :: (Volume -> b), 
  _voiceNote :: NoteNumber
}


-- calls the lens for _voiceFiltEnv filtEnv 
--  using typeclasses so multiple different types can have a filtEnv
makeFields ''Voice

instance Steppable Pulse (Voice a b) where
  step dt =  do
    -- run the filter envelope and update the filter frequency
    stepFilterEnv dt

    -- run the oscillator and the volume envelope
    pulse <-  step dt .@ osc
    vol <- step dt .@ venv

    -- run the filter
    output <- runFilter (pulse*vol) .@ filt

    return $ output

  -- run the filter envelope and update the filter frequency
stepFilterEnv :: Seconds -> State (Voice a b) ()
stepFilterEnv dt = do
  filterFreqOffset <- step dt .@ filtEnv
  f <- use filtEnvCurve
  filt.param .= f filterFreqOffset


-- =======================================================================

initialiseVoice :: FreqField a => Voice a b -> NoteNumber -> Voice a b
initialiseVoice v noteNum = v & osc . freq .~ (hzFromNoteNumber noteNum)
                              & note .~ noteNum

-- #TODO since voice has a NoteNumber, should this take note number and do nothing if they don't match?
releaseVoice :: Voice a b -> Voice a b
releaseVoice = (venv %~ noteOffEnv) . (filtEnv %~ noteOffEnv)

restartVoice :: Voice a b -> Voice a b
restartVoice = (venv %~ restartEnv) . (filtEnv %~ restartEnv)

-- #TODO default voice could be better thought through
defaultVoice :: Voice FreqParam Hz
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
