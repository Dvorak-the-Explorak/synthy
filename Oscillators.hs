{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TemplateHaskell
           , TypeSynonymInstances
           , BangPatterns
           , ExistentialQuantification
  #-}

module Oscillators where

-- An Oscillator has a stateful action that produces `Pulse`s.  
--  it has hidden internal state (usually phase of a waveform), 
--  as well as exposed parameters.  

-- An Oscillator can be a primitive wave (sine, square, saw,...)
--  or something loaded from a WAVE file (wavetable, sample)


import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import System.Random

import General
import Data.Fixed (mod')
import Wavetable (Wavetable)
import Helpers
import Steppable
import Parameterised

import Debug.Trace




newtype SimpleOscStore = SimpleOscStore (Phase, Hz)
type SimpleOsc = (Kernel SimpleOscStore Seconds Pulse)

instance FreqField SimpleOscStore where
  freq = (lens get set) . _2
    where
      get (SimpleOscStore s) = s 
      set _ x = SimpleOscStore x


newtype WavetableOscStore = WavetableOscStore (Phase, WaveIndex, Hz)
type WavetableOsc = Kernel WavetableOscStore Seconds Pulse

instance FreqField WavetableOscStore where
  freq = (lens get set) . _3
    where
      get (WavetableOscStore s) = s 
      set _ x = WavetableOscStore x

instance WaveIndexField WavetableOscStore where
  waveIndex = (lens get set) . _2
    where
      get (WavetableOscStore s) = s
      set (WavetableOscStore s) x = WavetableOscStore x


updatePhase dt freq_ phase_ = (`mod'` 1.0) $ phase_ + dt*freq_
 

simpleOscReader :: Waveform -> (Seconds -> State SimpleOscStore Pulse)
simpleOscReader wf = \dt -> do
  SimpleOscStore (_phase, _freq) <- get
  let _phase' = updatePhase dt _freq _phase
  put $ SimpleOscStore (_phase', _freq)
  return $ wf _phase'

-- -- Wavetable :: (WaveIndex -> Phase -> Pulse)
-- wavetableReader :: Wavetable -> OscReader Phase WavetableParam
-- wavetableReader f  = 
--   \(WavetableParam (waveIndex_,freq_)) dt -> 
--     modify (updatePhase dt freq_) >> gets (f waveIndex_)

-- Wavetable :: (WaveIndex -> Phase -> Pulse)
wavetableReader :: Wavetable -> (Seconds -> State WavetableOscStore Pulse)
wavetableReader f = \dt -> do
  WavetableOscStore (_phase, _waveIndex, _freq) <- get
  let _phase' = updatePhase dt _freq _phase
  put $ WavetableOscStore (_phase', _waveIndex, _freq)
  return $ f _waveIndex _phase


randomOscReader :: RandomGen g => (Seconds -> State g Pulse)
randomOscReader = \dt -> state $ uniformR (0.0, 1.0)
  


-- ============================================================


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

zeroOsc :: SimpleOsc
zeroOsc = simpleOsc (const 0)

lfo1s :: SimpleOsc
lfo1s = simpleOsc pureTone & freq .~ 1


simpleOsc :: Waveform -> SimpleOsc
simpleOsc wf = Kernel 
  { _storage = SimpleOscStore (0,0)
  , _doStep = simpleOscReader wf }

sawOsc = simpleOsc sawTone
squareOsc = simpleOsc squareTone
sineOsc = simpleOsc pureTone


wavetableOsc :: Wavetable -> WavetableOsc
wavetableOsc table =  Kernel
  { _storage = WavetableOscStore (0, 0, 0)
  , _doStep = wavetableReader table
  }

-- whiteNoiseOsc :: RandomGen g => g -> Oscillator ()
-- whiteNoiseOsc g = Oscillator
--   { _getSample = randomOscReader
--   , _oscStorage = g
--   , _oscParams = ()
--   }

whiteNoiseOsc :: RandomGen g => g -> Kernel g Seconds Pulse
whiteNoiseOsc g = Kernel
  { _storage = g
  , _doStep = randomOscReader
  }

-- =============================================================


-- Mix 2 oscillators together 50/50
-- ignores second oscillator's parameter
-- mix :: Oscillator a -> Oscillator a -> Oscillator a
-- mix (Oscillator get1 store1 p1) (Oscillator get2 store2 _) = 
--   (Oscillator getSample (store1,store2) p1)
--     where 
--       getSample param dt = do
--         out1 <- get1 param dt .@ _1
--         out2 <- get2 param dt .@ _2
--         return (0.5*out1 + 0.5*out2)


-- Mix 2 oscillators together 50/50
mix :: Kernel a Seconds Pulse -> Kernel b Seconds Pulse -> Kernel (a,b) Seconds Pulse
mix (Kernel s1 doStep1) (Kernel s2 doStep2) = 
  (Kernel (s1,s2) _doStep)
    where 
      _doStep dt = do
        out1 <- doStep1 dt .@ _1
        out2 <- doStep2 dt .@ _2
        return (0.5*out1 + 0.5*out2)



noisy :: RandomGen g => g -> Volume -> Kernel s Seconds Pulse -> Kernel (s, g) Seconds Pulse
noisy g noiseMix (Kernel _storage _doStep) = (Kernel _storage' _doStep')
  where
    _storage' = (_storage, g)

    _doStep' dt = do
      output <- _doStep dt .@ _1
      noise <- randomOscReader dt .@ _2
      return ((1-noiseMix)*output + noiseMix*noise)


