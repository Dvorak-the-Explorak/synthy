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
import Debug.Trace
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
data Synth v = Synth {
  _synthVoices :: Map.Map NoteNumber v, 
  _synthFilt :: Filter Float,
  _synthLfo :: SimpleOsc,
  _synthLfoStrength :: Float,
  _synthVoiceTemplate :: v
}

data AnySynth = forall v . (Source v, FreqField v, IsVoice v) => AnySynth (Synth v)

makeFields ''Synth



instance Steppable Seconds Pulse AnySynth where
  step dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (step dt) synth
    in (output, AnySynth synth')


-- Source == Steppable Seconds Pulse
instance (Source s, IsVoice s) => Steppable Seconds Pulse (Synth s) where
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
    filt.param += strength*moduland

    -- -- modulate wavetable indices
    -- -- modify $ over voices $ map (osc.waveIndex .~ (moduland+1)/2)
    -- -- voices %= map (osc.waveIndex .~ (moduland+1)/2)
    -- voices.each.osc.waveIndex .= (moduland+1)/2

    -- -- -- run the filter to get the output
    output <- step pulse .@ filt
    -- let output = pulse

    -- unmodulate the filter cutoff
    filt.param -= strength*moduland

    -- give some headroom 
    return $ 0.1*output

-- ===================================================================================




-- #TODO find a workaround for all this boilerplate
instance IsSynth AnySynth where
  _runSynthANiente dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynthANiente dt) synth
    in (output, AnySynth synth')


  _runSynthSteps n dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynthSteps n dt) synth
    in (output, AnySynth synth')

  _runSynth dt = state $ \(AnySynth synth) -> let
      (output, synth') = runState (runSynth dt) synth
    in (output, AnySynth synth')



  _noteOnSynth note vel = state $ \(AnySynth synth) -> let
      (output, synth') = runState (noteOnSynth note vel) synth
    in (output, AnySynth synth')

  _noteOffSynth note = state $ \(AnySynth synth) -> let
      (output, synth') = runState (noteOffSynth note) synth
    in (output, AnySynth synth')

  _noteOffAllSynth = state $ \(AnySynth synth) -> let
      (output, synth') = runState noteOffAllSynth synth
    in (output, AnySynth synth')



-- Kill any remaining notes, wait for them to ring out
runSynthANiente :: (Source s, IsVoice s) => Seconds -> State (Synth s) [Pulse]
runSynthANiente dt = do
  noteOffAllSynth
  iterateStateUntil (uses voices null) (step dt)

runSynthSteps :: (Source s, IsVoice s) => Int -> Seconds -> State (Synth s) [Pulse]
runSynthSteps n dt = iterateState n (step dt)

-- Chunks the timestep into at most 1 second long chunks
--    this helps when voices end early,
--    as they're only culled once per call to runSynthSteps
--    and leaving them in the EnvDone state will waste time calculating zeros
-- #TODO could the EnvDone state be signalled somehow to automate the culling?
runSynth :: (Source s, IsVoice s) => Seconds -> State (Synth s) [Pulse]
runSynth dt | dt < (1.0/sampleRate) = return []
            | dt > 1.0 = do 
                firstSec <- runSynthSteps (floor sampleRate) (1.0/sampleRate)
                remainder <- runSynth (dt - 1.0)
                return $ firstSec ++ remainder
            | otherwise = let n = floor $ dt*sampleRate
                      in runSynthSteps n (1.0/sampleRate)



noteOnSynth :: (Source s, FreqField s, IsVoice s) => 
                    NoteNumber -> Volume -> State (Synth s) ()
noteOnSynth note vel = do
  newVoice <- initialise note vel <$> use voiceTemplate
  voices %= Map.insertWith (flip const) note newVoice

noteOffSynth :: IsVoice s => NoteNumber -> State (Synth s) ()
noteOffSynth note = voices %= Map.adjust release note

noteOffAllSynth :: IsVoice s => State (Synth s) ()
noteOffAllSynth = voices.each %= release


-- ==============================================================================

simpleSynth :: SimpleOsc -> AnySynth
simpleSynth osc = AnySynth $ defaultSynth & voiceTemplate.source .~ osc



-- defaultSynth :: Synth (Voice SimpleOsc (Filter FreqParam))
defaultSynth = Synth {
  _synthVoices = Map.empty, 
  -- _synthFilt = bandPass (1/sampleRate) & param .~ (220, 880),--param is (low, high)
  -- _synthFilt = centeredBandPass (1/sampleRate) & param .~ (440, 220),--param is (center, width)
  -- _synthFilt = lowPass (1/sampleRate) & param . freq .~ 440,--param is cutoff frequency
  -- _synthFilt = cubicFilter & param .~ 0.8,--param is clip limit
  _synthFilt = clipper,--param is clip limit
  -- _synthFilt = hashtagNoFilter (0,0),
  _synthLfo = lfo1s & freq .~ 0.4,
  _synthLfoStrength = 0.2, -- 400 * 10,
  -- _synthLfoStrength = 400,
  _synthVoiceTemplate = defaultVoice
}


