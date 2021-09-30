{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Oscillators where

import Control.Monad.State
import Control.Lens

import General (Phase, Pulse, Hz, Seconds)
import Data.Fixed (mod')

import Helpers

-- #TODO add stepOsc as a record field
-- #TODO actually things should be more typeclasses than records? esp. the state operations...


type WaveFunction = Phase -> Pulse
type Waveform = State Oscillator Pulse

data Oscillator = Oscillator {
  _wave :: Waveform,
  _phase :: Phase,
  _freq :: Hz,
  _waveIndex :: Phase
}

-- makes the lenses, calls the lens for _wave just wave
makeLenses ''Oscillator

-- ==========================================


pureTone :: WaveFunction
pureTone = (sin . (*) (2*pi))

sawTone :: WaveFunction
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: WaveFunction
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)



-- ============================================================

waveFuncFromSamples :: [Float] -> WaveFunction
waveFuncFromSamples vals = \x -> let
    step = 1.0 / (fromIntegral $ length vals - 1)
    i = (floor $ (x/step)) `mod` (length vals)
    next = (i+1) `mod` (length vals)
    frac = (x - (fromIntegral i)*step)/step
  in (vals !! i) + frac * ((vals !! next) - (vals !! i))

-- ==========================================================

makeWaveform :: WaveFunction -> Waveform
makeWaveform f = uses phase f

zeroOsc :: Oscillator
zeroOsc = Oscillator {
  _wave = makeWaveform $ const 0,
  _phase = 0,
  _freq = 0,
  _waveIndex = 0
}

lfo1s :: Oscillator
lfo1s = zeroOsc & wave .~ makeWaveform pureTone 
                & freq .~ 1

simpleOsc :: WaveFunction -> Oscillator
simpleOsc wf = zeroOsc & wave .~ makeWaveform wf

sawOsc = simpleOsc sawTone
squareOsc = simpleOsc squareTone
sineOsc = simpleOsc pureTone




-- ==============================================

stepOsc :: Seconds -> State Oscillator Pulse
stepOsc dt = do
  wave_ <- use wave
  phase_ <- use phase -- `use` is `view` on the state
  freq_ <- use freq
  let newPhase = flip mod' 1.0 $ phase_ + dt*freq_
  assign phase newPhase -- `assign` is `set` on the state

  output <- wave_
  return $ 0.1 * output
-- stepOsc dt = state $ \os -> let newPhase = flip mod' 1.0 $ os ^. phase + dt*(os ^. freq)
--                               in ( (*0.1) $ os ^. wave $ newPhase, os & phase .~ newPhase)


-- should basically act like iterating stepOsc N times
runOsc :: Int -> Seconds -> State Oscillator [Pulse]
runOsc 0 dt = return []
runOsc n dt = do
  pulse <- stepOsc dt
  pulses <- runOsc (n-1) dt
  return $ pulse:pulses

-- runOsc n dt = do
--   wave_ <- use wave
--   phase_ <- use phase
--   freq_ <- use freq
--   -- let nextPhase = flip mod' 1.0 $ phase_ + dt*freq_*(fromIntegral n)
--   let phases = map (\i -> (dt*freq_*(fromIntegral i)) + phase_) [1..n]
--   let nextPhase = flip mod' 1.0 $ last phases
--   OOPS wave_ doesn't take the phase as input, it reads it from the Oscillator state
--   outputs <- mapState wave_ phases
--   -- put $ osc & phase .~ nextPhase
--   phase .= nextPhase
--   return outputs