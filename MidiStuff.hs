module MidiStuff where
-- {-# LANGUAGE DataKinds #-}
import General


type NoteNumber = Int
type ChannelNumber = Int

hzFromNoteNumber :: NoteNumber -> Hz
hzFromNoteNumber num = 440.0 * 2 ** ((fromIntegral num - 69.0)/12.0) 


data MidiEvent = NoteOn NoteNumber Velocity | NoteOff NoteNumber | PitchBend Int
data MidiMessage = Broadcast Seconds | ChannelMessage Seconds ChannelNumber MidiEvent 

type MidiSequence = [MidiMessage]

