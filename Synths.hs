{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes
           , ExistentialQuantification
  #-}

module Synths where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Control.Lens
import Data.List (sum)

import Data.Map (Map)
import qualified Data.Map as Map hiding (insertWith, adjust)
import qualified Data.Map.Strict as Map

import Codec.Midi

import General (Seconds, Pulse, sampleRate, Hz, Volume)
import Voices
-- import Voices (Voice(..), IsVoice, initialiseVoice, defaultVoice, 
--           stepVoices, noteOnVoicesWith, noteOffVoices, releaseVoice,
--           note, voiceFinished, cullVoices)
import Filters
import Helpers ((.@), stateMap, injectState, mapWhere, iterateState, iterateStateUntil)
import MidiStuff (NoteNumber)
import Envelopes (VolEnv(..), EnvSegment(..), currentState)
import Oscillators
import Steppable
import Parameterised


import Debug.Trace


-- #TODO can this be neater so I don't need all this shit???
class IsSynth s where
  _runSynthSteps :: Int -> Seconds -> State s [Pulse]
  _runSynthANiente :: Seconds -> State s [Pulse]
  _runSynth :: Seconds -> State s [Pulse]
  _noteOnSynth :: NoteNumber -> Volume -> State s ()
  _noteOffSynth :: NoteNumber -> State s ()
  _noteOffAllSynth :: State s ()

-- Synth represents one polyphonic instrument (homogenous voice types)
-- Synth is just a [Voice] with a global filter, modulated by LFO


-- type parameter v is the type of voice...
data Synth v f = Synth {
  _synthVoices :: Map.Map NoteNumber v, 
  _synthFilt :: f,
  _synthLfo :: SimpleOsc,
  _synthLfoStrength :: Float,
  _synthVoiceTemplate :: v
}

data AnySynth = forall v f. (Source v, IsVoice v, Transformer f, FreqField f) => 
                            AnySynth (Synth v f)

makeFields ''Synth



instance Steppable Seconds Pulse AnySynth where
  step dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (step dt) synth
    in (output, AnySynth synth')



-- #TODO put the LFO into the stepChunk
-- Source == Steppable Seconds Pulse
instance (Source v, IsVoice v, Transformer f, FreqField f) => Steppable Seconds Pulse (Synth v f) where
  step dt  = do
    -- if voices :: t v, then  pulses :: t Pulse
    -- pulses :: Map NoteNumber v
    pulses <- stateMap (step dt) .@ voices
    let pulse = Map.foldl' (+) 0 pulses

    voices %= Map.filter (not . finished)

    -- run the LFO
    moduland <- step dt .@ lfo
    strength <- use lfoStrength

    -- modulate the filter cutoff with the LFO
    filt.freq += strength*moduland

    -- -- modulate wavetable indices
    -- -- modify $ over voices $ map (osc.waveIndex .~ (moduland+1)/2)
    -- -- voices %= map (osc.waveIndex .~ (moduland+1)/2)
    -- voices.each.osc.waveIndex .= (moduland+1)/2

    -- -- -- run the filter to get the output
    output <- step pulse .@ filt
    -- let output = pulse

    -- unmodulate the filter cutoff
    filt.freq -= strength*moduland

    -- give some headroom 
    return $ 0.1*output

-- -- #TODO incorporate the LFO somehow
--   stepChunk dts = do
--     -- modulands <- stepChunk dts .@ lfo
--     -- strength <- use lfoStrength

--     pulseChunks <- stateMap (stepChunk dts) .@ voices
--     let pulseChunk = Map.foldl' (zipWith (+)) (repeat 0) pulseChunks

--     voices %= Map.filter (not . finished)

--     (map (0.1*)) <$> stepChunk pulseChunk .@ filt






modulated :: (Transformer f, FreqField f) => Hz -> State f Pulse -> State f Pulse
modulated change action = do
  freq += change
  result <- action
  freq -= change
  return result


-- ===================================================================================




-- #TODO find a workaround for all this boilerplate
instance IsSynth AnySynth where
  _runSynthANiente dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynthANiente dt) synth
    in (output, AnySynth synth')


  _runSynthSteps n dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynthSteps n dt) synth
    in (output, AnySynth synth')
  -- _runSynthSteps n dt = stepChunk $ take n $ repeat dt

  _runSynth dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynth dt) synth
    in (output, AnySynth synth')



  -- _noteOnSynth note vel = trace ("anysynth: " ++ show (note, vel)) $ state $ \(AnySynth synth) -> let
  --     (output, synth') = runState (noteOnSynth note vel) synth
  --   in (output, AnySynth synth')

  _noteOnSynth note vel = state $ \(AnySynth synth) -> ((), AnySynth $ execState (noteOnSynth note vel) synth)

  _noteOffSynth note = state $ \(AnySynth synth) -> let
      (output, synth') = runState (noteOffSynth note) synth
    in (output, AnySynth synth')

  _noteOffAllSynth = state $ \(AnySynth synth) -> let
      (output, synth') = runState noteOffAllSynth synth
    in (output, AnySynth synth')



-- Kill any remaining notes, wait for them to ring out
-- #TODO wait for ringing in the filters to finish, not just the voices
runSynthANiente :: (Source v, IsVoice v, Transformer f, FreqField f) => Seconds -> State (Synth v f) [Pulse]
runSynthANiente dt = do
  noteOffAllSynth
  iterateStateUntil (uses voices null) (step dt)

runSynthSteps :: (Source s, IsVoice s, Transformer f, FreqField f) => Int -> Seconds -> State (Synth s f) [Pulse]
runSynthSteps n dt = iterateState n (step dt)

-- Chunks the timestep into at most 1 second long chunks
--    this helps when voices end early,
--    as they're only culled once per call to runSynthSteps
--    and leaving them in the EnvDone state will waste time calculating zeros
-- #TODO could the EnvDone state be signalled somehow to automate the culling?
runSynth :: (Source s, IsVoice s, Transformer f, FreqField f) => Seconds -> State (Synth s f) [Pulse]
runSynth dt | dt < (1.0/sampleRate) = return []
            | dt > 1.0 = do 
                firstSec <- runSynthSteps (floor sampleRate) (1.0/sampleRate)
                remainder <- runSynth (dt - 1.0)
                return $ firstSec ++ remainder
            | otherwise = let n = floor $ dt*sampleRate
                      in runSynthSteps n (1.0/sampleRate)



noteOnSynth :: (Source v, IsVoice v) => 
                    NoteNumber -> Volume -> State (Synth v f) ()
noteOnSynth note vel = do
  newVoice <- initialise note vel <$> use voiceTemplate
  -- restart existing voice
  voices %= Map.insertWith (\ new old -> restart old) note newVoice

noteOffSynth :: IsVoice s => NoteNumber -> State (Synth s f) ()
noteOffSynth note = voices %= Map.adjust release note

noteOffAllSynth :: IsVoice s => State (Synth s f) ()
noteOffAllSynth = voices.each %= release


-- ==============================================================================

-- simpleSynth :: SimpleOsc -> AnySynth
-- simpleSynth osc = AnySynth $ defaultSynth & voiceTemplate.source .~ osc

-- simpleSynth :: (Source s) => s -> AnySynth
simpleSynth osc = AnySynth $ defaultSynth
                              { _synthVoices = Map.empty
                              , _synthVoiceTemplate = defaultVoice 
                                { _voiceSource = osc 
                                }
                              }


defaultSynth = Synth {
  _synthVoices = Map.empty, 
  -- _synthFilt = bandPass (1/sampleRate) & param .~ (220, 880),--param is (low, high)
  -- _synthFilt = centeredBandPass (1/sampleRate) & freq .~ 440
  --                                                 & bandwidth .~ 220
  -- _synthFilt = lowPass (1/sampleRate) & freq .~ 440,--param is cutoff frequency
  -- _synthFilt = cubicFilter & param .~ 0.8,--param is clip limit
  -- _synthFilt = centeredBandPass (1/sampleRate) & freq .~ 800,--param is clip limit
  _synthFilt = lowPass (1/sampleRate) & freq .~ 44000,--param is cutoff frequency
  -- _synthFilt = hashtagNoFilter (0,0),
  _synthLfo = lfo1s & freq .~ 0.4,
  _synthLfoStrength = 0.2, -- 400 * 10,
  -- _synthLfoStrength = 400,
  _synthVoiceTemplate = defaultVoice
}


