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

outputFile :: FilePath
outputFile = "output.bin"


performMidi :: Track Ticks -> [Pulse]
performMidi track = evalState (synthesiseMidiTrack track) defaultSynth

performMidiWithSynth :: FullSynth -> Track Ticks -> [Pulse]
performMidiWithSynth synth track = evalState (synthesiseMidiTrack track) synth

-- performMidiSaw :: Track Ticks -> [Pulse]
-- performMidiSaw track = performMidiWithSynth $ defaultSynth & 

-- =====================================================
-- =====================================================

main = do
  -- printMidi "maralinga.mid"
  putStrLn $ printf "Playing something"
  play "c_major.mid"
  -- printSong
  putStrLn $ "made " ++ outputFile


printSong :: IO ()
printSong = putStrLn $ concatMap ((++" ") . show) $ zip scaleDegrees freqs
  where
    scaleDegrees = [-7..21]
    freqs = map ionian19TET scaleDegrees

-- B.floatLE is float little endian
byteStringFromPulses :: [Pulse] -> B.ByteString
byteStringFromPulses pulses = B.toLazyByteString $ mconcat $ map B.floatLE pulses

save :: B.ByteString -> IO()
save song = saveAs outputFile song

saveAs :: FilePath -> B.ByteString -> IO()
saveAs path song = B.writeFile path song

printMidi :: FilePath -> IO ()
printMidi inputFile = do
  parsedMidi <- fmap (runParser parseMidi) $ B.readFile inputFile
  case parsedMidi of
    (Left errorString) -> putStrLn errorString
    (Right midi) -> do
      putStrLn $ show $ timeDiv midi
      putStrLn $ (show $ length $ tracks midi) ++ " tracks found"
      mapM (mapM putStrLn . map show) $ tracks midi
      return ()


play :: FilePath -> IO ()
play inputFile = do

  parsedMidi <- fmap (runParser parseMidi) $ B.readFile inputFile
  case parsedMidi of
    (Left errorString) -> putStrLn errorString
    (Right midi) -> do
      putStrLn $ show $ timeDiv midi
      putStrLn $ show $ length $ tracks midi
      -- mapM putStrLn $ map show $ head $ tracks midi 
      -- mapM putStrLn $ map (show . length) $ tracks midi 

      -- pulses = map hardClip $ performToyMidi testSeq2
      -- #TODO interpret the midi TimeDiv and TempoChange messages to work out the time steps
      let pulsesFromTracks track = performMidi $ map (first (*24)) track
      let outputs = map pulsesFromTracks $ tracks midi
      let pulses = map (hardClip . sum) $ transpose outputs

      save $ byteStringFromPulses pulses
      -- runCommand :: String -> IO (processHandleOrSomething)
      _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate outputFile
      return ()

--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3