import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import Text.Printf
import Data.List
import Data.Fixed
import Data.Tuple.Extra
import Control.Monad.State
-- =========================
import General
import MidiStuff
import Songs
import Synths
import Scales
import Envelopes
import Helpers
-- import Debug.Trace

-- #TODO play from MIDI file
-- #TODO percussion sounds - white noise


data Pitch = Pitch {
    pitchClass :: Int,
    octave :: Int
}

instance Show Pitch where
    show (Pitch n oct) | n >=0 && n < 12 = (noteNames !! n) ++ (show oct)
                      | otherwise = (noteNames !! (n `mod` 12)) ++ (show $ oct + n `div` 12)

-- enharmonic spellings ignored
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- pitchFromString str | (str !! 1) == '#' = Pitch (find (== take 2 str) noteNames) (read $ drop 2 str)
--                     | otherwise = Pitch (find (== take 1 str) noteNames) (read $ drop 1 str)


freqFromPitch :: Pitch -> Hz
freqFromPitch (Pitch 9 4) = 440.0
freqFromPitch (Pitch pc oct) = 2.0**((fromIntegral oct)-4 + ((fromIntegral pc)-9)/12.0) * (freqFromPitch $ Pitch 9 4)
                             

-- ================================================================
-- ================================================================

filename :: FilePath
filename = "output.bin"

tempo :: Beats
tempo = 133.0

tonalCenter :: Pitch
tonalCenter = Pitch 9 4

-- ================================

addSounds :: [Pulse] -> [Pulse] -> [Pulse]
addSounds = zipWith (+)

-- ==================================================================================


-- =====================================================
-- =====================================================

performSequence :: Synth -> Scale ->  Pitch -> Sequence -> [Pulse]
performSequence synth scale pitch notes = concat $ map makeChord notes
    where
        makeChord :: ([ScaleDegree], Seconds) -> [Pulse]
        makeChord ([], d) = replicate (floor $ sq * d * sampleRate) 0.0
        makeChord (ns, d) =  map sum $ transpose $ map (\n -> makeNote n d) ns
        -- makeChord (ns, d) = let 
        --                         sounds = map (\n -> makeNote n d) ns
        --                         combined [] = []
        --                         combined xs = (sum $ map head xs):combined (map tail xs)
        --                     in combined sounds

        makeNote n d = env 1.0 (sq*d) $ synth ((freqFromPitch pitch) * (scale n)) (sq * d)
        env :: Envelope
        env = adsr (sq/8) (sq/2) 0.6 (sq/10)
        sq = 60.0/tempo/4.0

-- --          startTime  note
-- type Voice = (Seconds, NoteNumber)
-- type InstrumentState = (Seconds, [Voice])



-- -- #TODO how will we put envelopes in here
-- performMidiSequence :: ChannelNumber -> Oscillator -> MidiSequence -> [Pulse]
-- performMidiSequence channel osc messages = [0.0]
--     where
--         runSynth :: [Seconds] -> Voice -> [Pulse]
--         runSynth ts (startTime, num) = map (osc . (*hz)) tsRelative
--             where
--                 tsRelative = map (\t -> t-startTime) ts
--                 hz = hzFromNoteNumber num

--         stepSynths :: Seconds -> State InstrumentState [Pulse]
--         stepSynths dt = do
--             (t, voices) <- get
--             let ts = map ((+t) . (/sampleRate)) $ tail [0.0..dt*sampleRate]
--             let sounds = map (runSynth ts) voices
--             return (map sum $ transpose sounds)

--         readMessage :: MidiMessage -> State InstrumentState [Pulse]
--         readMessage (Broadcast dt) = stepSynths dt
--         -- readMessage (ChannelMessage dt ch event) 
--         --         | ch /= channel = stepSynths dt
--         --         | otherwise = do
--         --             case event of
--         --                 NoteOn num vel -> modify (\(t, voices) -> (t, (t,num):voices))
--         --                 NoteOff num -> modify (second $ filter $ \(_, n) -> n /= num)
--         --                 PitchBend val -> undefined
--         --             stepSynths dt
--         readMessage (ChannelMessage dt ch event) = do
--             if ch == channel
--             then case event of

--                     NoteOn num vel -> modify (\(t, voices) -> (t, (t,num):voices))
--                     NoteOff num -> modify (second $ filter $ \(_, n) -> n /= num)
--                     PitchBend val -> undefined
--             else pure ()
--             stepSynths dt














-- =====================================================
-- =====================================================

main = do
    putStrLn $ printf "Playing song in %s" $ show tonalCenter 
    play
    putStrLn $ "made " ++ filename

-- B.floatLE is float little endian
song = B.toLazyByteString $ mconcat $ map B.floatLE wave

wave :: [Pulse]
-- wave = performSequence defaultSynth dorian tonalCenter jump
-- wave = performSequence defaultSynth ionian tonalCenter silentNight
-- wave = jumpTour
-- wave = jumpTour2
wave = silentNightFull locrian
-- wave = jumpFull phrygian
-- wave = performSequence pureSynth ionian tonalCenter silentNightChords

silentNightFull :: Scale -> [Pulse]
silentNightFull modality = map (/3) $ addSounds bassline $ addSounds melody chords 
    where
        chords = map (*2) $ performSequence pureSynth modality tonalCenter silentNightChords
        melody = performSequence sawSynth modality tonalCenter silentNightMelody
        bassline = performSequence squareSynth modality tonalCenter silentNightBassline

jumpFull :: Scale -> [Pulse]
jumpFull modality = map (/2) $ addSounds bassline chords
    where
        bassline = performSequence squareSynth modality tonalCenter jumpBassline
        chords = performSequence sawSynth modality tonalCenter jump


jumpTour :: [Pulse]
jumpTour = mconcat $ map (\scale -> performSequence defaultSynth scale tonalCenter jump) [ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian]


jumpTour2 :: [Pulse]
jumpTour2 = mconcat $ map (\scale -> performSequence defaultSynth scale tonalCenter jump) [lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian]


save :: IO()
save = saveAs filename

saveAs :: FilePath -> IO()
saveAs path = B.writeFile path song

play :: IO()
play = do
    save
    -- runCommand :: String -> IO (processHandleOrSomething)
    _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate filename
    return ()

