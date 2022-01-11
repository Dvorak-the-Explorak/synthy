{-# LANGUAGE DeriveGeneric
           , FlexibleContexts    
           , FlexibleInstances
           , MultiParamTypeClasses   
           , TemplateHaskell          
           , FunctionalDependencies
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

-- for types that have hidden storage (which doesn't expose a parameter),
--  as well as a type that does expose parameters
-- #TODO need a better name than "ParamSecond"
newtype ParamSecond a b = ParamSecond (a, b) 
  deriving Generic
instance Wrapped (ParamSecond a b)
-- want these instances so we can use `ParamSecond a b` just like `(a,b)`
instance Field1 (ParamSecond a b) (ParamSecond a b) a a where
  _1 = _Wrapped' . _1
instance Field2 (ParamSecond a b) (ParamSecond a b) b b where
  _2 = _Wrapped' . _2

data WithStorage s a = WithStorage
  { _withStorageStorage :: s
  , _withStorageParam :: a
  }


-- calls the lens for `_withStorageStorage` just "storage"
makeFields ''WithStorage



-- #TODO do all this shit with TemplateHaskell?
-- class FieldClass Name t s | Name -> t where
--   name :: Setter' s t

-- makeFieldClass Name t 
--   becomes
-- class NameField s where
--   field :: Setter' s t
-- lifting instances 




class FreqField s where
  freq :: Lens' s Hz

instance FreqField FreqParam where
  freq = _Wrapped'  

instance FreqField WavetableParam where
  freq = _Wrapped' . _2

instance FreqField b => FreqField (ParamSecond a b) where
  freq = _2 . freq

instance FreqField a => FreqField (WithStorage s a) where
  freq = param . freq

-- instance (FreqField a, FreqField b) => FreqField (BothParams a b) where
--   freq = Setter $ \ (BothParams a b) x -> BothParams (a & freq .~ x) (b & freq .~ x)

instance (FreqField a, FreqField b) => FreqField (a,b) where
  freq = lens get set
    where
      -- no guarantee a and b have the same freq value unless (a,b) just got set
      get (a,b) = a ^. freq  

      set (a,b) x = (a & freq .~ x, b & freq .~ x)



instance FreqField s => FreqField (Kernel s i o) where
  freq = storage . freq



-- ===========================================================================================

class WaveIndexField s where
  waveIndex :: Setter' s WaveIndex

instance WaveIndexField WavetableParam where
  waveIndex = _Wrapped' . _1



instance WaveIndexField b => WaveIndexField (ParamSecond a b) where
  waveIndex = _2 . waveIndex

instance WaveIndexField a => WaveIndexField (WithStorage s a) where
  waveIndex = param . waveIndex



instance (WaveIndexField a, WaveIndexField b) => WaveIndexField (a,b) where
  waveIndex = sets $ \ f (a,b) -> (over waveIndex f a, over waveIndex f b)

instance WaveIndexField s => WaveIndexField (Kernel s i o) where
  waveIndex =  storage . waveIndex


-- ================================================================================

class BandwidthField a where
  bandwidth :: Setter' a Float

instance BandwidthField b => BandwidthField (ParamSecond a b) where
  bandwidth = _2 . bandwidth

instance BandwidthField a => BandwidthField (WithStorage s a) where
  bandwidth = param . bandwidth

instance (BandwidthField a, BandwidthField b) => BandwidthField (a,b) where
  bandwidth = sets $ \ f (a,b) -> (over bandwidth f a, over bandwidth f b)


instance BandwidthField s => BandwidthField (Kernel s i o) where
  bandwidth =  storage . bandwidth