{-# LANGUAGE FlexibleContexts #-}


module MidiSequencer where

import Control.Monad.State
import Control.Lens
import Codec.Midi
import Steppable
import Voices
import Parameterised


import Oscillators
import General
import Synths




performMidi :: Track Ticks -> [Pulse]
performMidi = performMidiSquare

performMidiWithSynth :: (Source s, FreqField s, IsVoice s) =>  Synth s -> Track Ticks -> [Pulse]
performMidiWithSynth synth track = evalState (synthesiseMidiTrack track) synth

performMidiSaw :: Track Ticks -> [Pulse]
performMidiSaw = performMidiWithOscillator sawOsc
performMidiSquare = performMidiWithOscillator squareOsc
performMidiSine = performMidiWithOscillator sineOsc

performMidiWithOscillator :: SimpleOsc -> Track Ticks -> [Pulse]
performMidiWithOscillator osc_ = performMidiWithSynth $ defaultSynth & voiceTemplate.source .~ osc_




-- ================================================================================
--                        MIDI Interpretation
-- ================================================================================


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