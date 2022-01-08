module Parameterised where

import Control.Lens

import General


newtype FreqParam = FreqParam Hz
newtype WavetableParam = WavetableParam (WaveIndex, Hz)


class FreqField s where
  freq :: Setter' s Hz

instance FreqField FreqParam where
  freq = lens get set
    where
      get (FreqParam f) = f
      set (FreqParam _) x = FreqParam x

instance FreqField WavetableParam where
  freq = lens get set
    where
      get (WavetableParam (_,f)) = f
      set (WavetableParam (wi,_)) x = WavetableParam (wi,x)




class WaveIndexField s where
  waveIndex :: Setter' s WaveIndex


instance WaveIndexField WavetableParam where
  waveIndex = lens get set
    where
      get (WavetableParam (wi,_)) = wi
      set (WavetableParam (_,f)) x = WavetableParam (x,f)


