{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , BangPatterns
  #-}

module Oscillators where

-- An Oscillator stores its current wave index and phase, 
--  as well as its frequency and a function that outputs its pulse

-- An Oscillator can be a primitive wave (sine, square, saw,...)
--  or something loaded from a WAVE file (wavetable, sample)


import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import General (Phase, Pulse, Hz, Seconds, WaveIndex)
import Data.Fixed (mod')
import Wavetable (Wavetable)
import Helpers
import Steppable

import Debug.Trace

-- #TODO turn stepOsc into a class instance method


-- pure waveform, can evaluate its pulse from just phase
type Waveform = Phase -> Pulse
-- also takes wave index
type OscReader = WaveIndex -> Phase -> Pulse


data Oscillator = Oscillator {
  _getSample :: OscReader,
  _phase :: Phase,
  _freq :: Hz,
  _waveIndex :: Phase
} 


-- makes the lenses, calls the lens for _getSample just getSample
makeLenses ''Oscillator

-- ==========================================

instance Steppable Pulse Oscillator where
  step dt = do
    getSample_ <- use getSample
    phase_ <- use phase -- `use` is `view` on the state
    freq_ <- use freq
    let newPhase = (`mod'` 1.0) $ phase_ + dt*freq_
    phase .= newPhase -- operator notation for assign ("set" but for state)

    waveIndex_ <- use waveIndex
    let output = getSample_ waveIndex_ newPhase
    return  output



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

-- class Steppable s a where
-- step :: Seconds -> State s a
-- run :: Int -> Seconds -> State s [a]

