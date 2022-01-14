{-# LANGUAGE FlexibleContexts
            , TemplateHaskell
            , RankNTypes    
            , ExistentialQuantification               
#-}


module MidiSequencer where

import Control.Monad.State
import Control.Lens
import Codec.Midi hiding (Velocity)
import Data.Tuple.Extra
import Data.List (transpose)

import Data.Map.Strict (Map)
-- import qualified Data.Map as Map hiding (insertWith, adjust)
import qualified Data.Map.Strict as Map

import Steppable
import Voices
import Parameterised
import Oscillators
import General 
import Helpers
import Synths
import MidiStuff

import Debug.Trace

-- #TODO need to make a SynthesiseMidi function which does the whole Midi object (not just a track)
--      - might need a map from channel/instrument to synth

type PatchLookup = Int -> AnySynth

data TrackState = TrackState 
  { _channels :: Map Int AnySynth
  , _trackVolume :: Map Int Volume 
  }


makeLenses ''TrackState


performMidi :: Track Ticks -> [Pulse]
performMidi = performMidiSquare


performMidiTrack :: Track Ticks -> [Pulse]
performMidiTrack = performMidiWithSynth defaultSynth

performMidiWithSynth :: (Source s, FreqField s, IsVoice s) =>  Synth s -> Track Ticks -> [Pulse]
performMidiWithSynth synth track = evalState (synthesiseMidiTrack track) synth

performMidiSaw :: Track Ticks -> [Pulse]
performMidiSaw = performMidiWithOscillator sawOsc
performMidiSquare = performMidiWithOscillator squareOsc
performMidiSine = performMidiWithOscillator sineOsc


performMidiWithOscillator :: SimpleOsc -> Track Ticks -> [Pulse]
performMidiWithOscillator osc = performMidiWithSynth $ defaultSynth & voiceTemplate.source .~ osc




-- ================================================================================
--                        MIDI Interpretation
-- ================================================================================

runOnChannels :: State AnySynth [Pulse] -> State TrackState [Pulse]
runOnChannels action = do
  outputs <- stateMap action .@ channels

  vols <- use trackVolume
  let channelVolume chNum = (Map.findWithDefault 1.0 chNum vols)
  
  let outputs' = Map.elems $ Map.mapWithKey (\ chNum pulses -> map (* (channelVolume chNum)) pulses) $ outputs  
  return $ map (hardClip . sum) $ transpose outputs'


runChannels :: Int -> Seconds -> State TrackState [Pulse]
runChannels n dt = runOnChannels (_runSynthSteps n dt)

runChannelsANiente :: Seconds -> State TrackState [Pulse] 
runChannelsANiente dt = runOnChannels (_runSynthANiente dt)

noteOnTrack :: Int -> NoteNumber -> Velocity -> State TrackState ()
-- noteOnTrack ch noteNum vel = Map.adjust (_noteOnSynth noteNum vel) ch .@ channels
noteOnTrack ch noteNum vel = channels %= Map.adjust (execState $ _noteOnSynth noteNum vel) ch

noteOffTrack :: Int -> NoteNumber -> State TrackState ()
noteOffTrack ch noteNum = channels %= Map.adjust (execState $ _noteOffSynth noteNum) ch

noteOffAllTrack :: Int -> State TrackState ()
noteOffAllTrack ch = channels %= Map.adjust (execState _noteOffAllSynth) ch










samplesPerTick :: Midi -> Float
samplesPerTick midi = case timeDiv midi of
  -- samplesPerTick = samplesPerSecond / ticksPerSecond
  -- samplesPerTick = samplesPerSecond / (beatsPerSecond * ticksPerBeat)
  -- samplesPerTick = samplesPerSecond / (2 *ticksPerBeat)
  TicksPerBeat n -> sampleRate / (fromIntegral (2 * n))
  -- samplesPerTick = samplesPerSecond / ticksPerSecond
  -- samplesPerTick = samplesPerSecond / (framesPerSecond * ticksPerFrame)
  TicksPerSecond framesPerSecond ticksPerFrame -> sampleRate / (fromIntegral (framesPerSecond * ticksPerFrame))
  

synthesiseMidi :: (Int -> AnySynth) -> Midi -> [Pulse]
synthesiseMidi getPatch midi = let
    -- #TODO flooring the rate will cause misalignment problems 
    --      when the sample rate and tick rate don't match well
    timeScale = floor $ samplesPerTick midi
    
    -- pulses = performMidiTrack $ map (first (*timeScale)) $ head $ tail $ tracks midi

    trackPulses = map (performMidiTrack . map (first (*timeScale))) $ tracks midi
    pulses = map (hardClip . sum) $ transpose trackPulses
  in pulses


notImplemented :: Monad m => Message -> m ()
notImplemented x = trace ("Unhandled Midi: " ++ show x) $ return ()




synthesiseMidiTrackNew :: Track Ticks -> State TrackState [Pulse]
-- when there are no more messages, let all the sounds finish
synthesiseMidiTrackNew [] = runChannelsANiente (1/sampleRate)
synthesiseMidiTrackNew ((ticks, TrackEnd):_) = runChannels ticks (1/sampleRate) -- don't run aNiente
synthesiseMidiTrackNew ((ticks, message):messages) = do
  -- `ticks` says how many steps since the previous message, so run the synth that long
  output <- runChannels ticks (1/sampleRate)

  -- handle the effect of the message
  case message of
    NoteOff ch key vel -> noteOffTrack ch key 
    -- NoteOn with 0 velocity counts as noteOff
    NoteOn ch key 0 -> noteOffTrack ch key 
    NoteOn ch key vel -> noteOnTrack ch key $ (fromIntegral vel) / 127.0
    x -> notImplemented x 

  -- do the rest of the messages
  (output++) <$> synthesiseMidiTrackNew messages


synthesiseMidiTrack :: (Source s, FreqField s, IsVoice s) =>  Track Ticks -> State (Synth s) [Pulse]
-- when there are no more messages, let all the sounds finish
synthesiseMidiTrack [] = runSynthANiente (1/sampleRate)
synthesiseMidiTrack ((ticks, TrackEnd):_) = runSynthSteps ticks (1/sampleRate) -- don't run aNiente
synthesiseMidiTrack ((ticks, message):messages) = do
  -- `ticks` says how many steps since the previous message, so run the synth that long
  output <- runSynthSteps ticks (1/sampleRate)

  -- handle the effect of the message
  case message of
    NoteOff ch key vel -> noteOffSynth key 
    -- NoteOn with 0 velocity counts as noteOff
    NoteOn ch key 0 -> noteOffSynth key 
    NoteOn ch key vel -> noteOnSynth key $ (fromIntegral vel) / 127.0
    x -> notImplemented x 

    -- KeyPressure ch key pressure -> return ()
    -- ControlChange ch contNum contVal -> return ()
    -- ProgramChange ch preset -> return ()
    -- ChannelPressure ch pressure -> return ()
    -- PitchWheel ch pitch -> return ()
    -- SequenceNumber n -> return ()
    -- Text str -> return ()   
    -- Copyright str -> return ()  
    -- TrackName str -> return ()  
    -- InstrumentName str -> return ()   
    -- Lyrics str -> return ()   
    -- Marker str -> return ()   
    -- CuePoint str -> return ()   
    -- ChannelPrefix ch -> return ()   
    -- ProgramName str -> return ()  
    -- DeviceName str -> return ()   
    -- TrackEnd -> return () 
    -- TempoChange tempo -> return ()
    -- -- SMPTEOffset !Int !Int !Int !Int !Int   
    -- -- TimeSignature !Int !Int !Int !Int  
    -- -- KeySignature !Int !Int   
    -- -- Reserved !Int !ByteString  
    -- -- Sysex !Int !ByteString
    -- _ -> return ()

  -- do the rest of the messages
  (output++) <$> synthesiseMidiTrack messages

  -- remainder <- synthesiseMidiTrack messages
  -- return $ output ++ remainder

