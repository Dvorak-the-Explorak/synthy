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
import Debug.Trace

-- #TODO interpret more midi messages:
--    TempoChange and TimeDiv to get the speed of songs right
--    ProgramChange to get different instruments?
--    Velocity in NoteOn and NoteOff
-- #TODO percussion sounds - white noise
-- #TODO voices with samples - load up a general midi soundfont and use that
-- #TODO more filters:
--    Reverb
--    lowPass with resonance
--    highPass with resonance
--    bandPass
--    phaser


-- #TODO live stream mode, plug a midi control

-- ================================================================


x_squared :: [Float]
x_squared = map ((\x -> x*x) . (/256)) [0..256]

-- ================================================================

outputFile :: FilePath
outputFile = "output.bin"


performMidi :: Track Ticks -> [Pulse]
performMidi = performMidiSquare

performMidiWithSynth :: FullSynth -> Track Ticks -> [Pulse]
performMidiWithSynth synth track = evalState (synthesiseMidiTrack track) synth



performMidiSaw :: Track Ticks -> [Pulse]
-- performMidiSaw = performMidiWithSynth $ over makeVoice (\f -> (\n -> (f n) & osc . wave .~ sawTone)) defaultSynth
-- performMidiSaw = performMidiWithSynth $ over makeVoice (\f -> (set (osc.wave) sawTone) . f) defaultSynth
performMidiSaw = performMidiWithWaveform sawTone

performMidiSquare = performMidiWithWaveform squareTone
performMidiPure = performMidiWithWaveform pureTone

performMidiWithWaveform :: Waveform -> Track Ticks -> [Pulse]
-- performMidiWithWaveform wf  = performMidiWithSynth $ over makeVoice (\f -> (\n -> (f n) & osc . wave .~ wf)) defaultSynth
-- performMidiWithWaveform wf  = performMidiWithSynth $ over makeVoice (\f -> (set (osc.wave) wf) . f) defaultSynth
performMidiWithWaveform wf = performMidiWithSynth $ over makeVoice ((set (osc.wave) wf) . ) defaultSynth

-- =====================================================
-- =====================================================

main = do
  let wave = waveformFromSamples x_squared
  putStrLn $ show $ wave 1
  putStrLn $ show $ wave 0.1

  -- printMidi "mario2_overworld.mid"
  
  -- putStrLn $ printf "Playing something"
  -- play "pokemon_rb_center.mid"
  -- -- playOnabots
  -- putStrLn $ "made " ++ outputFile


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


playOnabots :: IO ()
playOnabots = do
  parsedMidi <- fmap (runParser parseMidi) $ B.readFile "onabots_2.mid"
  case parsedMidi of
    (Left errorString) -> putStrLn errorString
    (Right midi) -> do
      putStrLn $ show $ timeDiv midi
      putStrLn $ show $ length $ tracks midi

      -- data

      -- sawtooth, 
      -- pure, 
      -- sawtooth, 
      -- square
      -- pure
      -- pure / square 
      -- drum

      -- #TODO interpret the midi TimeDiv and TempoChange messages to work out the time steps
      let pulsesFromTracks track = performMidi $ map (first (*24)) track

      let timeScale = first (*24)


      let v1 = performMidiSaw $ map timeScale $ (tracks midi) !! 1 
      let v2 = performMidiPure $ map timeScale $ (tracks midi) !! 2 
      let v3 = performMidiSaw $ map timeScale $ (tracks midi) !! 3
      let v4 = performMidiSquare $ map timeScale $ (tracks midi) !! 4
      let v5 = performMidiPure $ map timeScale $ (tracks midi) !! 5
      let v6 = performMidiSquare $ map timeScale $ (tracks midi) !! 6 
      let v7 = performMidiSquare $ map timeScale $ (tracks midi) !! 7
      let outputs = [v1, v2, v3 ,v4, v5, v6, v7]
      let pulses = map (hardClip . sum) $ transpose outputs


      -- let drums = performMidiSquare $ map timeScale $ (tracks midi) !! 
      -- let pulses = map hardClip drums

      save $ byteStringFromPulses pulses
      -- runCommand :: String -> IO (processHandleOrSomething)
      _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate outputFile
      return ()

play :: FilePath -> IO ()
play inputFile = do
  parsedMidi <- fmap (runParser parseMidi) $ B.readFile inputFile
  case parsedMidi of
    (Left errorString) -> putStrLn errorString
    (Right midi) -> do
      putStrLn $ show $ timeDiv midi
      putStrLn $ (show $ length $ tracks midi) ++ " tracks"
      -- mapM putStrLn $ map show $ head $ tracks midi 
      -- mapM putStrLn $ map (show . length) $ tracks midi 

      let timeScale = first (*60)

      -- pulses = map hardClip $ performToyMidi testSeq2
      -- #TODO interpret the midi TimeDiv and TempoChange messages to work out the time steps
      let pulsesFromTracks track = performMidi $ map timeScale track
      let outputs = map pulsesFromTracks $ tracks midi
      let pulses = map (hardClip . sum) $ transpose outputs

      save $ byteStringFromPulses pulses
      -- runCommand :: String -> IO (processHandleOrSomething)
      _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate outputFile
      return ()

--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3