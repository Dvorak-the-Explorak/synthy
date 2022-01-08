{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

module Envelopes where

import Control.Monad.State
import Control.Lens

import General
import Steppable



data EnvSegment = EnvAttack | EnvDecay | EnvSustain | EnvRelease | EnvDone
  deriving (Eq, Ord, Enum, Show)
data VolEnv = VolEnv {
  _attackSlope :: Float,
  _decaySlope :: Float,
  _sustainLevel :: Float,
  _releaseSlope :: Float,
  _currentState :: EnvSegment,
  _volume :: Volume
}

-- makes the lenses, calls the lens for _attackSlope just attackSlope
makeLenses ''VolEnv

instance Steppable Seconds Volume VolEnv where
  step dt = do
    slope <- gets envSlope
    vol <- use volume 
    -- let slope = envSlope venv
    -- let nextVol = (dt*slope) + (venv ^. volume)
    let nextVol = vol + (dt*slope)
    venv <- get
    if timestepWithinSegment venv dt
      then (put $ venv & volume .~ nextVol) >> return nextVol
      else withState toNextSegment $ step (dt - timeToSegmentTarget venv)

-- Run the envelope N samples forward
runEnv :: Int -> Seconds -> State VolEnv [Volume]
runEnv 0 dt = return []
runEnv n dt = do
  venv <- get
  let slope = envSlope venv
  let steps = take n $ tail $ iterate (+(dt*slope)) (venv ^. volume)
  let valid = takeWhile (stillInSegment venv) steps
  nextSegmentSteps <- if length valid == n
                        then (put $ venv & volume .~ last valid) >> return []
                        -- then state $ \venv -> ([], venv {_volume = last valid})
                        else withState toNextSegment $ runEnv (n - (length valid)) dt
  return $ valid ++ nextSegmentSteps


-- =============================================================================
-- ======================== Functions ==========================================
-- =============================================================================


envSlope :: VolEnv -> Float
envSlope venv = case _currentState venv of
  EnvAttack -> _attackSlope venv
  EnvDecay -> -(_decaySlope venv)
  EnvSustain -> 0.0
  EnvRelease -> -(_releaseSlope venv)
  EnvDone -> 0.0
stillInSegment :: VolEnv -> Float -> Bool
stillInSegment venv x = case _currentState venv of
  EnvAttack -> (x <= 1.0)
  EnvDecay -> (x >= (_sustainLevel venv))
  EnvSustain -> True
  EnvRelease -> (x >= 0.0)
  EnvDone -> True
-- just jump to next segment.
--  if _volume hadn't reached the threshold for the next segment, just jump to it
--    - should only be a small amount between samples
toNextSegment :: VolEnv -> VolEnv
toNextSegment venv = case _currentState venv of
  EnvAttack -> venv {_volume = 1.0, _currentState = EnvDecay}
  EnvDecay -> venv {_volume = _sustainLevel venv, _currentState = EnvSustain}
  EnvSustain -> venv {_currentState = EnvRelease}
  EnvRelease -> venv {_volume = 0.0, _currentState = EnvDone}
  EnvDone -> venv
timeToSegmentTarget :: VolEnv -> Seconds
timeToSegmentTarget venv = case _currentState venv of
  EnvAttack -> if _attackSlope venv > 0
                then (1 - _volume venv) / (_attackSlope venv)
                else 0
  EnvDecay -> if _decaySlope venv > 0
                then (_volume venv - _sustainLevel venv)/(_decaySlope venv)
                else 0
  EnvSustain -> 0.0
  EnvRelease -> if _releaseSlope venv > 0 
                then (_volume venv)/(_releaseSlope venv)
                else 0
  EnvDone -> 0.0
timestepWithinSegment :: VolEnv -> Seconds -> Bool
timestepWithinSegment venv dt = case _currentState venv of
  EnvAttack -> dt <= timeToSegmentTarget venv
  EnvDecay -> dt <= timeToSegmentTarget venv
  EnvSustain -> True
  EnvRelease -> dt <= timeToSegmentTarget venv
  EnvDone -> True

restartEnv :: VolEnv -> VolEnv
restartEnv venv = venv & currentState .~ EnvAttack

-- jump to decay section of envelope
noteOffEnv :: VolEnv -> VolEnv
noteOffEnv venv = venv & currentState .~ EnvRelease


-- ================================================
-- ============ State operations ==================
-- ================================================


-- class Steppable s a where
-- step :: Seconds -> State s a
-- run :: Int -> Seconds -> State s [a]
