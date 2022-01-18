{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TemplateHaskell
           , TypeSynonymInstances
           , BangPatterns
           , ExistentialQuantification
           , DeriveGeneric
  #-}

module Oscillators where

{- 
Oscillator is no longer a distinct type, they are represented through Kernel types
Oscillators are basic sound sources, which can be statefully stepped to produce a pulse 
    (ie. they are instances of Steppable Second Pulse)
  and can expose parameters through lenses (see module Parameterised.hs)
An Oscillator could be:
  a primitive wave (sine, square, saw,...)
  a waveform loaded from a file
  a wavetable loaded from a file
-}


import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import System.Random
-- import Data.List.Extra ((!?))
import GHC.Generics

import General
import Data.Fixed (mod')
import Wavetable (Wavetable)
import Helpers
import Steppable
import Parameterised


import qualified Data.Vector as V
import Data.Vector (Vector, (!))

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




newtype OneshotOscStore = OneshotOscStore (Vector Pulse, Seconds, Seconds)
  deriving Generic
instance Wrapped OneshotOscStore
newtype OneshotOsc = OneshotOsc (Kernel OneshotOscStore Seconds Pulse)
  deriving Generic
instance Wrapped OneshotOsc


-- ======================================================================


updatePhase dt freq_ phase_ = (`mod'` 1.0) $ phase_ + dt*freq_
 

stepSimpleOsc :: Waveform -> (Seconds -> State SimpleOscStore Pulse)
stepSimpleOsc wf = \dt -> do
  SimpleOscStore (_phase, _freq) <- get
  let _phase' = updatePhase dt _freq _phase
  put $ SimpleOscStore (_phase', _freq)
  return $ wf _phase'

stepWavetableOsc :: Wavetable -> (Seconds -> State WavetableOscStore Pulse)
stepWavetableOsc f = \dt -> do
  WavetableOscStore (_phase, _waveIndex, _freq) <- get
  let _phase' = updatePhase dt _freq _phase
  put $ WavetableOscStore (_phase', _waveIndex, _freq)
  return $ f _waveIndex _phase


stepRandomOsc :: RandomGen g => (Seconds -> State g Pulse)
stepRandomOsc = \dt -> state $ uniformR (0.0, 1.0)
  

stepOneshotOsc :: Seconds -> State OneshotOscStore Pulse
stepOneshotOsc = \dt -> do
  OneshotOscStore (pulses, rate, t) <- get
  let t' = t + dt
  let index = floor $ t' * rate
  let output = if index >= V.length pulses 
                then 0.0
                else pulses ! index
  put $ OneshotOscStore (pulses, rate, t')
  return output


-- for IsVoice instance
oneshotRestart osc = osc & _Wrapped' . storage . _Wrapped' . _3 .~ 0
oneshotRelease (OneshotOsc (Kernel (OneshotOscStore (pulses, rate, _)) go)) = 
                OneshotOsc $ Kernel (OneshotOscStore (pulses, rate, (fromIntegral $ V.length pulses) / rate)) go
oneshotFinished  (OneshotOsc (Kernel (OneshotOscStore (pulses, rate, t)) go)) = 
                    t > (fromIntegral $ V.length pulses) / rate
oneshotInitialise _ vol (OneshotOsc (Kernel (OneshotOscStore (pulses, rate, t)) go)) = 
                   (OneshotOsc $ Kernel (OneshotOscStore ((V.map (*vol) pulses), rate, t)) go)

-- ============================================================


pureTone :: Waveform
pureTone = (sin . (*) (2*pi))

sawTone :: Waveform
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Waveform
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else 1.0)

-- ============================================================


waveformFromSamples :: [Pulse] -> Waveform
waveformFromSamples vals = \x -> let
    step = 1.0 / (fromIntegral $ length vals - 1)
    i = (floor $ (x/step)) `mod` (length vals)
    next = (i+1) `mod` (length vals)
    frac = (x - (fromIntegral i)*step)/step
  in (vals !! i) + frac * ((vals !! next) - (vals !! i))

makeOneshot :: [Pulse] -> Seconds -> OneshotOsc
makeOneshot pulses sampleRate = OneshotOsc $ Kernel s go
  where
    s = OneshotOscStore (V.fromList pulses, sampleRate, 0)
    go = stepOneshotOsc

instance Steppable Seconds Pulse OneshotOsc where
  step dt = step dt .@ _Wrapped'



-- ==========================================================

zeroOsc :: SimpleOsc
zeroOsc = simpleOsc (const 0)

lfo1s :: SimpleOsc
lfo1s = simpleOsc pureTone & freq .~ 1


simpleOsc :: Waveform -> SimpleOsc
simpleOsc wf = Kernel s go
  where
    s = SimpleOscStore (0,0)
    go = stepSimpleOsc wf

sawOsc = simpleOsc sawTone
squareOsc = simpleOsc squareTone
sineOsc = simpleOsc pureTone


wavetableOsc :: Wavetable -> WavetableOsc
wavetableOsc table =  Kernel s go
  where
    s = WavetableOscStore (0, 0, 0)
    go = stepWavetableOsc table


whiteNoiseOsc :: RandomGen g => g -> Kernel g Seconds Pulse
whiteNoiseOsc g = Kernel s go
  where
    s = g
    go = stepRandomOsc
  

-- =============================================================


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
      noise <- stepRandomOsc dt .@ _2
      return ((1-noiseMix)*output + noiseMix*noise)


