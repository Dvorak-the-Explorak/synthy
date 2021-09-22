module MidiStuff where
-- {-# LANGUAGE DataKinds #-}
import General

type NoteNumber = Int
type ChannelNumber = Int


-- no microtonal stuff anymore :(
hzFromNoteNumber :: NoteNumber -> Hz
hzFromNoteNumber num = 440.0 * 2 ** ((fromIntegral num - 69.0)/12.0) 

-- =============================================================================================
-- toy version 
data ToyMidi = ToyNoteOn NoteNumber Seconds | ToyNoteOff NoteNumber Seconds | ToyNothing Seconds
