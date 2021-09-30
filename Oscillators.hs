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

-- #TODO add stepOsc as a record field
-- #TODO actually things should be more typeclasses than records? esp. the state operations...


type Waveform = Phase -> Pulse


data Oscillator = Oscillator {
  _wave :: Waveform,
  _phase :: Phase,
  _freq :: Hz,
  _waveIndex :: Phase
}

-- makes the lenses, calls the lens for _wave just wave
makeLenses ''Oscillator

-- ==========================================


pureTone :: Waveform
pureTone = (sin . (*) (2*pi))

sawTone :: Waveform
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Waveform
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)



-- ============================================================

waveformFromSamples :: [Float] -> Waveform
waveformFromSamples vals = \x -> let
    step = 1.0 / (fromIntegral $ length vals - 1)
    i = (floor $ (x/step)) `mod` (length vals)
    next = (i+1) `mod` (length vals)
    frac = (x - (fromIntegral i)*step)/step
  in (vals !! i) + frac * ((vals !! next) - (vals !! i))

-- ==========================================================

zeroOsc :: Oscillator
zeroOsc = Oscillator {
  _wave = const 0,
  _phase = 0,
  _freq = 0,
  _waveIndex = 0
}

lfo1s :: Oscillator
lfo1s = zeroOsc & wave .~ pureTone 
                & freq .~ 1

makeOsc :: Waveform -> Oscillator
makeOsc wf = zeroOsc & wave .~ wf

sawOsc = makeOsc sawTone
squareOsc = makeOsc squareTone
sineOsc = makeOsc pureTone




-- ==============================================

stepOsc :: Seconds -> State Oscillator Pulse
-- stepOsc dt = do
--   phase_ <- use phase -- `use` is `view` on the state
--   freq_ <- use freq
--   wave_ <- use wave
--   let newPhase = flip mod' 1.0 $ phase_ + dt*freq_
--   assign phase newPhase -- `assign` is `set` on the state
--   return $ 0.1 * (wave_ newPhase)
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