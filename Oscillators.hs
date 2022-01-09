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

-- pure waveform, can evaluate its pulse from just phase
type Waveform = Phase -> Pulse

-- also takes wave index
type OscReader s a = a -> Seconds -> State s Pulse


-- #TODO this mirrors the Filter data type very closely, 
--  there could some abstraction to be had here
data Oscillator a = forall s . Oscillator {
  _getSample :: OscReader s a,
  _oscStorage :: s,
  _oscParams :: a
} 


type SimpleOscillatorOld = Oscillator (FreqParam)

newtype SimpleOscStore = SimpleOscStore { unSimpleOscStore :: (Phase, Hz) }

type SimpleOsc = (Kernel SimpleOscStore Seconds Pulse)


-- makes the lenses, calls the lens for _getSample just getSample
makeLenses ''Oscillator

instance FreqField SimpleOscStore where
  freq = lens get set
    where
      get (SimpleOscStore s) = snd s 
      set (SimpleOscStore (p,_)) x = SimpleOscStore (p,x)


-- ==========================================

-- step :: Seconds -> State (Oscillator a) Pulse
instance Steppable Seconds Pulse (Oscillator a) where
  step dt = state $ \(Oscillator getSample s param) -> let
      (output, s') = runState (getSample param dt) s 
    in (output, Oscillator getSample s' param)


-- if the oscillator's parameter exposes a waveIndex,
--  so too does the oscillator
instance WaveIndexField p => WaveIndexField (Oscillator p) where
  waveIndex = oscParams . waveIndex

-- if the oscillator's parameter exposes a frequency,
--  so too does the oscillator
instance FreqField p => FreqField (Oscillator p)where
  freq = oscParams . freq

-- ==================================================


updatePhase dt freq_ phase_ = (`mod'` 1.0) $ phase_ + dt*freq_

-- stores the phase, freq as param
-- Waveform :: Phase -> Pulse
simpleOscReaderOld :: Waveform -> OscReader Phase FreqParam
simpleOscReaderOld f = 
  \(FreqParam freq_) dt -> 
    modify (updatePhase dt freq_) >> gets f 

simpleOscReader :: Waveform -> (Seconds -> State SimpleOscStore Pulse)
simpleOscReader wf = \dt -> do
  SimpleOscStore (_phase, _freq) <- get
  let _phase' = updatePhase dt _freq _phase
  put $ SimpleOscStore (_phase', _freq)
  return $ wf _phase'

-- Wavetable :: (WaveIndex -> Phase -> Pulse)
wavetableReader :: Wavetable -> OscReader Phase WavetableParam
wavetableReader f  = 
  \(WavetableParam (waveIndex_,freq_)) dt -> 
    modify (updatePhase dt freq_) >> gets (f waveIndex_)


randomOscReader :: RandomGen g => OscReader g ()
randomOscReader = \ _ dt -> state $ uniformR (0.0, 1.0)
  


-- ============================================================


pureTone :: Waveform
pureTone = (sin . (*) (2*pi))

sawTone :: Waveform
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Waveform
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)

-- ============================================================

-- -- they're the same type
-- wavetableReader :: Wavetable -> OscReader
-- wavetableReader = id        

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

simpleOscOld :: Waveform -> SimpleOscillatorOld
simpleOscOld wf =  Oscillator 
  { _getSample = simpleOscReaderOld wf
  , _oscStorage = 0 -- phase
  , _oscParams = FreqParam 0
  }

simpleOsc :: Waveform -> SimpleOsc
simpleOsc wf = Kernel 
  { _storage = SimpleOscStore (0,0)
  , _doStep = simpleOscReader wf }

sawOsc = simpleOsc sawTone
squareOsc = simpleOsc squareTone
sineOsc = simpleOsc pureTone

wavetableOsc :: Wavetable -> Oscillator WavetableParam
wavetableOsc table =  Oscillator 
  { _getSample = wavetableReader table
  , _oscStorage = 0 -- phase
  , _oscParams = WavetableParam (0, 0)
  }

whiteNoiseOsc :: RandomGen g => g -> Oscillator ()
whiteNoiseOsc g = Oscillator
  { _getSample = randomOscReader
  , _oscStorage = g
  , _oscParams = ()
  }

-- =============================================================


-- Mix 2 oscillators together 50/50
-- ignores second oscillator's parameter
mix :: Oscillator a -> Oscillator a -> Oscillator a
mix (Oscillator get1 store1 p1) (Oscillator get2 store2 _) = 
  (Oscillator getSample (store1,store2) p1)
    where 
      getSample param dt = do
        out1 <- get1 param dt .@ _1
        out2 <- get2 param dt .@ _2
        return (0.5*out1 + 0.5*out2)



-- noisy :: RandomGen g => g -> Volume -> Oscillator a -> Oscillator a
-- noisy g noiseMix (Oscillator getSample store param) = (Oscillator getSample' store' param)
--   where
--     store' = (store, g)
--     getSample' param dt = do
--       output <- getSample param dt .@ _1
--       noise <- randomOscReader () dt .@ _2
--       return ((1-noiseMix)*output + noiseMix*noise)

noisy :: RandomGen g => g -> Volume -> Kernel s Seconds Pulse -> Kernel (s, g) Seconds Pulse
noisy g noiseMix (Kernel _storage _doStep) = (Kernel _storage' _doStep')
  where
    _storage' = (_storage, g)

    _doStep' dt = do
      output <- _doStep dt .@ _1
      noise <- randomOscReader () dt .@ _2
      return ((1-noiseMix)*output + noiseMix*noise)


