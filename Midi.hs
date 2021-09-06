module Midi where
import General


type NoteNumber = Int



data MidiEvent = NoteOn NoteNumber Velocity | NoteOff NoteNumber Velocity | PitchBend Int
type MidiSequence = [MidiEvent]