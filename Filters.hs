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
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Vector.Mutable (write)

import General (Pulse, Hz, Volume, Seconds)
import Steppable
import Helpers
import Parameterised

type Filter a = Kernel a Pulse Pulse

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



-- -- Ring buffer style thing
-- newtype ConvFilter = ConvFilter (Kernel ConvHistory Pulse Pulse)
--   deriving Generic
-- instance Wrapped ConvFilter

-- instance Steppable Pulse Pulse ConvFilter where
--   step pulse = step pulse .@ _Wrapped'
--   -- stepChunk pulses = 

-- -- #TODO remove this, it's a lie
-- instance FreqField ConvFilter where
--   freq = lens get set
--     where
--       -- get _ = 0.0
--       get = const 0.0
--       -- set x _ = x
--       set = const

-- data ConvHistory = ConvHistory 
--   { vals :: Vector Pulse
--   , headIndex :: Int
--   }

-- roll :: Pulse -> ConvHistory -> ConvHistory
-- roll next (ConvHistory pulses n) = ConvHistory pulses' n'
--   where
--     n' = (n-1 + V.length pulses) `mod` V.length pulses
--     pulses' = V.modify (\v -> write v n' next) pulses
-- getDot :: [Pulse] -> ConvHistory -> Pulse
-- getDot kernel (ConvHistory pulses n) = result
--   where
--     (histEnd, histStart) = V.splitAt n pulses
--     result = sum $ zipWith (*) kernel $ V.toList histStart ++ V.toList histEnd


-- convolution :: [Pulse] -> Int -> ConvFilter
-- convolution kernel n = ConvFilter $ Kernel (ConvHistory (V.replicate n 0.0) 0) go
--   where
--     go pulse = do
--       modify $ roll pulse 
--       -- dot product of kernel with history
--       gets $ getDot kernel

-- ===========================================





-- #TODO do a better convolution (this is uselessly slow)
-- convolution :: Vector Pulse -> Int -> Kernel (Vector Pulse) Pulse Pulse
-- convolution kernel n = Kernel (V.fromList $ take n $ repeat 0.0) go
--   where
--     go pulse = do

--       modify $! V.zipWith (+) (V.map (*pulse) kernel)
--       output <- gets V.head
--       modify $! (flip V.snoc 0.0) . V.tail
--       return output
convolution :: [Pulse] -> Int -> Kernel [Pulse] Pulse Pulse
convolution kernel n = Kernel [] go
  where
    go pulse = do
      modify $ take n . (pulse:)
      gets $ sum . zipWith (*) kernel

-- convolution :: [Pulse] -> Int -> Kernel [[Pulse]] Pulse Pulse
-- convolution kernel n = Kernel [] go
--   where
--     go pulse = do
--       modify $ ((map (*pulse) kernel):)
--       output <- gets $ sum . map head
--       modify $ filter (/= []) . map tail
--       -- return output
--       return output


-- makeConvolution :: [Pulse] -> Kernel [Pulse] Pulse Pulse
-- makeConvolution kernel = convolution (V.fromList kernel) (length kernel)
makeConvolution kernel = convolution kernel (length kernel)




-- ====================================================

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
