{-# LANGUAGE DeriveGeneric
           , FlexibleContexts    
           , FlexibleInstances
           , MultiParamTypeClasses             
#-}

module Parameterised where

import Control.Lens
import GHC.Generics

import General
import Steppable




newtype FreqParam = FreqParam Hz
  deriving Generic
instance Wrapped FreqParam

newtype WavetableParam = WavetableParam (WaveIndex, Hz)
  deriving Generic
instance Wrapped WavetableParam

newtype ParamSecond a b = ParamSecond (a, b) 
  deriving Generic
instance Wrapped (ParamSecond a b)
-- want these instances so we can use `ParamSecond a b` just like `(a,b)`
instance Field1 (ParamSecond a b) (ParamSecond a b) a a where
  _1 = _Wrapped' . _1
instance Field2 (ParamSecond a b) (ParamSecond a b) b b where
  _2 = _Wrapped' . _2






class FreqField s where
  freq :: Setter' s Hz

instance FreqField FreqParam where
  freq = _Wrapped'  

instance FreqField WavetableParam where
  freq = _Wrapped' . _2

instance FreqField b => FreqField (ParamSecond a b) where
  freq = _2 . freq



instance FreqField s => FreqField (Kernel s i o) where
  freq = storage . freq



-- ===========================================================================================

class WaveIndexField s where
  waveIndex :: Setter' s WaveIndex

instance WaveIndexField WavetableParam where
  waveIndex = _Wrapped' . _1

instance WaveIndexField s => WaveIndexField (Kernel s i o) where
  waveIndex =  storage . waveIndex


instance WaveIndexField b => WaveIndexField (ParamSecond a b) where
  waveIndex = _2 . waveIndex