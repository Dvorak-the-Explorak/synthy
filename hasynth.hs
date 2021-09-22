{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}
import Prelude hiding (unzip)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import Text.Printf
import Data.List hiding (unzip)
import Data.Fixed (mod')
import Data.Tuple.Extra
import Control.Monad.State
import Control.Functor.HT (unzip)
import Control.Lens
import Control.Lens.Tuple
-- =========================
import General
import MidiStuff
import Songs
import Oscillators
import Envelopes
import Filters
import Voices
import Synths
import Scales
import Helpers
import DiatonicSequencer
import Codec.Midi
import Codec.ByteString.Parser (runParser)
-- import Debug.Trace

-- #TODO play from MIDI file
-- #TODO percussion sounds - white noise


-- ================================================================
-- ================================================================

filename :: FilePath
filename = "output.bin"


performToyMidi :: [ToyMidi] -> [Pulse]
performToyMidi midi = evalState (synthesiseMidiFullSynth midi) defaultSynth

performMidi :: Track Ticks -> [Pulse]
performMidi track = evalState (synthesiseMidiTrack track) defaultSynth




testSeq1 :: [ToyMidi]
testSeq1 = [ToyNoteOn 69 0, ToyNoteOff 69 5,
            ToyNothing 5]

testSeq2 :: [ToyMidi]
testSeq2 = [ToyNoteOn 69 0, ToyNoteOn 73 0.5, ToyNoteOn 76 0.5, ToyNoteOn 81 0.5, 
            ToyNoteOff 69 2, ToyNoteOff 73 0, ToyNoteOff 76 0, ToyNoteOff 81 0,
            ToyNothing 10]


-- =====================================================
-- =====================================================

main = do

  putStrLn $ printf "Playing something"
  play
  -- printSong
  putStrLn $ "made " ++ filename


printSong :: IO ()
printSong = putStrLn $ concatMap ((++" ") . show) $ zip scaleDegrees freqs
  where
    scaleDegrees = [-7..21]
    freqs = map ionian19TET scaleDegrees

-- B.floatLE is float little endian
byteStringFromPulses :: [Pulse] -> B.ByteString
byteStringFromPulses pulses = B.toLazyByteString $ mconcat $ map B.floatLE pulses

save :: B.ByteString -> IO()
save song = saveAs filename song

saveAs :: FilePath -> B.ByteString -> IO()
saveAs path song = B.writeFile path song

play :: IO ()
play = do

  something <- fmap (runParser parseMidi) $ B.readFile "c_major.mid"
  case something of
    (Left errorString) -> putStrLn errorString
    (Right midi) -> do
      putStrLn $ show $ timeDiv midi
      mapM putStrLn $ map show $ head $ tracks midi 
      -- mapM putStrLn $ map (show . length) $ tracks midi 

      let firstTrack = head $ tracks midi

      -- pulses = map hardClip $ performToyMidi testSeq2
      -- #TODO interpret the midi TimeDiv and TempoChange messages to work out the time steps
      let pulses = map hardClip $ performMidi $ map (first (*100)) firstTrack

      save $ byteStringFromPulses pulses
      -- runCommand :: String -> IO (processHandleOrSomething)
      _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate filename
      return ()

--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3