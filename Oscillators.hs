{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , BangPatterns
  #-}

module Oscillators where

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import General (Phase, Pulse, Hz, Seconds)
import Data.Fixed (mod')
import Wavetable (Wavetable)
import Helpers

import Debug.Trace

-- #TODO add stepOsc as a record field
-- #TODO actually things should be more typeclasses than records? esp. the state operations...


type Waveform = Phase -> Pulse
type OscReader = Phase -> Phase -> Pulse

data Oscillator = Oscillator {
  _getSample :: OscReader,
  _phase :: Phase,
  _freq :: Hz,
  _waveIndex :: Phase
} 

-- makes the lenses, calls the lens for _getSample just getSample
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

-- they're the same type
wavetableReader :: Wavetable -> OscReader
wavetableReader = id            

waveformFromSamples :: [Float] -> Waveform
waveformFromSamples vals = \x -> let
    step = 1.0 / (fromIntegral $ length vals - 1)
    i = (floor $ (x/step)) `mod` (length vals)
    next = (i+1) `mod` (length vals)
    frac = (x - (fromIntegral i)*step)/step
  in (vals !! i) + frac * ((vals !! next) - (vals !! i))

-- ==========================================================

makeOscReader :: Waveform -> OscReader
makeOscReader f =  const f -- ignore first argument (waveIndex)

zeroOsc :: Oscillator
zeroOsc = Oscillator {
  _getSample = makeOscReader $ const 0,
  _phase = 0,
  _freq = 0,
  _waveIndex = 0
}

lfo1s :: Oscillator
lfo1s = zeroOsc & getSample .~ makeOscReader pureTone 
                & freq .~ 1

simpleOsc :: Waveform -> Oscillator
simpleOsc wf = zeroOsc & getSample .~ makeOscReader wf

sawOsc = simpleOsc sawTone
squareOsc = simpleOsc squareTone
sineOsc = simpleOsc pureTone

wavetableOsc :: Wavetable -> Oscillator
wavetableOsc table = zeroOsc & getSample .~ wavetableReader table

-- ==============================================

stepOsc :: Seconds -> State Oscillator Pulse
stepOsc dt = do
  getSample_ <- use getSample
  phase_ <- use phase -- `use` is `view` on the state
  freq_ <- use freq
  let newPhase = flip mod' 1.0 $ phase_ + dt*freq_
  -- assign phase newPhase -- `assign` is `set` on the state
  phase .= newPhase -- operator notation for assign ("set" but for state)

  waveIndex_ <- use waveIndex
  let output = getSample_ waveIndex_ newPhase
  return  output

-- should basically act like iterating stepOsc N times
runOsc :: Int -> Seconds -> State Oscillator [Pulse]
runOsc 0 dt = return []
runOsc n dt = do
  pulse <- stepOsc dt
  pulses <- runOsc (n-1) dt
  return $ pulse:pulses