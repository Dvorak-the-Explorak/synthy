{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , ExistentialQuantification
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , DeriveGeneric
  #-}

module Filters where

-- A Filter is a stateful object which can modify sequences of pulses 
--  it has internal state, and exposes parameters.  

import GHC.Generics

import Control.Monad.State
import Control.Lens

import General (Pulse, Hz, Volume, Seconds)
import Steppable
import Helpers
import Parameterised



(~>) :: Kernel s1 Pulse Pulse -> Kernel s2 Pulse Pulse -> Kernel (s1, s2) Pulse Pulse
(~>) = seqKernels

-- (+>) :: Filter a -> Filter b -> Filter (a,b)
-- (+>) = parallelFilters (+)

-- (*>) :: Filter a -> Filter b -> Filter (a,b)
-- (*>) = parallelFilters (*)

-- ========================================================================================

-- changing frequency shifts the center of the band, but keeps the bandwidth
bandPass dt = centeredBandPass dt

-- store the cutoffs, calculate the center and band for the lenses (`freq` gets center, `bandwidth` gets band width)
-- how is this the longest thing in the codebase what
data CBPStore a b = CBPStore a b
center :: (FreqField a, FreqField b) => CBPStore a b -> Hz
center (CBPStore a b) = (a ^. freq + b ^. freq) / 2
band :: (FreqField a, FreqField b) => CBPStore a b -> Hz
band (CBPStore a b) = (a ^. freq + b ^. freq) / 2

instance Field1 (CBPStore a b) (CBPStore a b) a a where
  _1 = lens get set
    where
      get (CBPStore p1 _) = p1
      set (CBPStore _ p2) x = CBPStore x p2
instance Field2 (CBPStore a b) (CBPStore a b) b b where
  _2 = lens get set
    where
      get (CBPStore _ p2) = p2
      set (CBPStore p1 _) x = CBPStore p1 x


instance (FreqField a, FreqField b) => FreqField (CBPStore a b) where
  freq = lens get set 
    where
      get s = center s
      set s x = s & _1 . freq .~ (x - (band s)/2)
                  & _2 . freq .~ (x + (band s)/2)
instance (FreqField a, FreqField b) => BandwidthField (CBPStore a b)  where
  bandwidth = lens get set
    where
      get s = band s
      set s x = s & _1 . freq .~ (center s - x/2) 
                  & _2 . freq .~ (center s + x/2)

-- centeredBandPass2 :: Seconds -> Kernel (CBPStore blah blah) Pulse Pulse
centeredBandPass dt = seqKernelsWith CBPStore (lowPass dt) (highPass dt)






lowPass :: Seconds -> Kernel (WithStorage Pulse FreqParam) Pulse Pulse
lowPass dt = Kernel s go
  where
    s = WithStorage 0 (FreqParam 0)
    go = lowPassFunc dt


highPass :: Seconds -> Kernel (WithStorage (Pulse,Pulse) FreqParam) Pulse Pulse
highPass dt = Kernel s go
  where
    s = WithStorage (0,0) $ FreqParam 0
    go = highPassFunc dt


hashtagNoFilter = Kernel () return

combFilter = Kernel s go 
  where
    s = CombStore ([], 0.8, 10)
    go = combFilterFunc

clipper :: Kernel Volume Pulse Pulse
clipper = Kernel 1.0 go
  where
    go pulse = do
      limit <- get
      return $ (/limit) $ hardClipLimit limit pulse

pureFilter :: (Pulse -> Pulse) -> Kernel () Pulse Pulse
pureFilter f = Kernel () (return . f)

cubicFilter :: Kernel Float Pulse Pulse
cubicFilter = Kernel 1.0 go
  where
    go pulse = do
      strength <- get
      return $ strength*pulse**3 + (1-strength)*pulse

gainFilter :: Kernel Float Pulse Pulse
gainFilter = Kernel 1.0 go
  where
    go pulse = do
      gain <- get
      return $ pulse * gain

-- ================================

rcFromCutoff :: Hz -> Float
rcFromCutoff f = 1/(2*pi*f)

--                         (--------------- Kernel go function -------------) 
lowPassFunc :: Seconds -> Pulse -> State (WithStorage Pulse FreqParam) Pulse
lowPassFunc dt = \pulse -> do
    WithStorage prev (FreqParam _freq) <- get
    let rc = rcFromCutoff _freq 
    let alpha = dt / (rc + dt)
    storage .= alpha * pulse + (1-alpha) * prev
    use storage

  --                        (--------------- Kernel go function -------------) 
highPassFunc :: Seconds -> Pulse -> State (WithStorage (Pulse,Pulse) FreqParam) Pulse
highPassFunc dt = \pulse -> do
    WithStorage (prevOut, prevIn) (FreqParam _freq) <- get
    let rc = rcFromCutoff _freq 
    let alpha = rc / (rc + dt)

    storage . _1 .= alpha * pulse + alpha * (prevOut - prevIn)
    storage . _2 .= pulse

    use $ storage . _1


data CombStore = CombStore ([Pulse], Float, Int)
  deriving Generic
instance Wrapped CombStore

-- Should this take delay as a Seconds parameter instead of samples?
combFilterFunc :: Pulse -> State CombStore Pulse
combFilterFunc pulse = do
  history <- use $ _Wrapped' . _1
  strength <- use $ _Wrapped' . _2
  let next = if null history 
          then pulse
          else pulse + strength * (head history)
  let n = length history
  delay <- use $ _Wrapped' . _3

  _Wrapped' . _1 .= if n < delay
                      then history ++ [next]
                      else (drop (n-delay+1) $ history) ++ [next]
  return next
