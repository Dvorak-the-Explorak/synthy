{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Oscillators where

import Control.Monad.State
import Control.Lens

import General(Phase, Pulse, Hz, Seconds)
import Data.Fixed (mod')


type Waveform = Seconds -> Pulse

pureTone :: Waveform
pureTone = (sin . (*) (2*pi))

sawTone :: Waveform
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Waveform
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)


-- ==========================================


data Oscillator = Oscillator {
  _wave :: Waveform,
  _phase :: Phase,
  _freq :: Hz
}

zeroOsc = Oscillator {
  _wave = const 0,
  _phase = 0,
  _freq = 0
}

lfo1s = Oscillator {
  _wave = pureTone,
  _phase = 0,
  _freq = 1
}


-- makes the lenses, calls the lens for _wave just wave
makeLenses ''Oscillator

-- ============================================================





-- ==============================================

stepOsc :: Seconds -> State Oscillator Pulse
stepOsc dt = state $ \os -> let newPhase = flip mod' 1.0 $ os ^. phase + dt*(os ^. freq)
                              in ( (*0.1) $ os ^. wave $ newPhase, os & phase .~ newPhase)

-- should basically act like iterating stepOsc N times
runOsc :: Int -> Seconds -> State Oscillator [Pulse]
runOsc 0 dt = return []
runOsc n dt = do
  osc <- get
  let nextPhase = flip mod' 1.0 $ (osc ^. phase) + dt*(osc ^. freq)*(fromIntegral n)
  let phases = map (\i -> (dt*(osc ^. freq)*(fromIntegral i)) + osc ^. phase) [1..n]
  let outputs = map (osc ^. wave) $ phases
  put $ osc & phase .~ nextPhase
  return outputs