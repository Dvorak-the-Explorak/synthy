{-# LANGUAGE FlexibleContexts #-}


module MidiSequencer where

import Control.Monad.State
import Control.Lens
import Codec.Midi
import Data.Tuple.Extra
import Data.List (transpose)

import Steppable
import Voices
import Parameterised
import Oscillators
import General
import Helpers
import Synths


-- #TODO need to make a SynthesiseMidi function which does the whole Midi object (not just a track)
--      - might need a map from channel/instrument to synth




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

samplesPerTick :: Midi -> Float
samplesPerTick midi = case timeDiv midi of
  -- samplesPerTick = samplesPerSecond / ticksPerSecond
  -- samplesPerTick = samplesPerSecond / (beatsPerSecond * ticksPerBeat)
  -- samplesPerTick = samplesPerSecond / (2 *ticksPerBeat)
  TicksPerBeat n -> sampleRate / (fromIntegral (2 * n))
  -- samplesPerTick = samplesPerSecond / ticksPerSecond
  -- samplesPerTick = samplesPerSecond / (framesPerSecond * ticksPerFrame)
  TicksPerSecond framesPerSecond ticksPerFrame -> sampleRate / (fromIntegral (framesPerSecond * ticksPerFrame))
  

synthesiseMidi :: Midi -> [Pulse]
synthesiseMidi midi = let
    -- #TODO flooring the rate will cause misalignment problems 
    --      when the sample rate and tick rate don't match well
    timeScale = floor $ samplesPerTick midi
    
    -- pulses = performMidiTrack $ map (first (*timeScale)) $ head $ tail $ tracks midi

    trackPulses = map (performMidiTrack . map (first (*timeScale))) $ tracks midi
    pulses = map (hardClip . sum) $ transpose trackPulses
  in pulses



synthesiseMidiTrack :: (Source s, FreqField s, IsVoice s) =>  Track Ticks -> State (Synth s) [Pulse]
-- when there are no more messages, let all the sounds finish
synthesiseMidiTrack [] = runSynthANiente (1/sampleRate)
synthesiseMidiTrack ((ticks, TrackEnd):_) = runSynthSteps ticks (1/sampleRate)
synthesiseMidiTrack ((ticks, message):messages) = do
  -- `ticks` says how many steps since the previous message, so run the synth that long
  output <- runSynthSteps ticks (1/sampleRate)

  -- handle the effect of the message
  case message of
    NoteOff ch key vel -> noteOffSynth key 
    NoteOn ch key 0 -> noteOffSynth key 
    NoteOn ch key vel -> noteOnSynth key

    KeyPressure ch key pressure -> return ()
    ControlChange ch contNum contVal -> return ()
    ProgramChange ch preset -> return ()
    ChannelPressure ch pressure -> return ()
    PitchWheel ch pitch -> return ()
    SequenceNumber n -> return ()
    Text str -> return ()   
    Copyright str -> return ()  
    TrackName str -> return ()  
    InstrumentName str -> return ()   
    Lyrics str -> return ()   
    Marker str -> return ()   
    CuePoint str -> return ()   
    ChannelPrefix ch -> return ()   
    ProgramName str -> return ()  
    DeviceName str -> return ()   
    TrackEnd -> return () 
    TempoChange tempo -> return ()
    -- SMPTEOffset !Int !Int !Int !Int !Int   
    -- TimeSignature !Int !Int !Int !Int  
    -- KeySignature !Int !Int   
    -- Reserved !Int !ByteString  
    -- Sysex !Int !ByteString
    _ -> return ()

  -- do the rest of the messages
  (output++) <$> synthesiseMidiTrack messages

  -- remainder <- synthesiseMidiTrack messages
  -- return $ output ++ remainder