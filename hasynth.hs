{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleContexts
  #-}
import Prelude hiding (unzip)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import System.Random
import Text.Printf
import Data.List hiding (unzip)
import Data.Fixed (mod')
import Data.Tuple.Extra
import Control.Monad.State
import Control.Functor.HT (unzip)
import Control.Lens
import Control.Lens.Tuple

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Codec.Midi
import Codec.ByteString.Parser (runParser, getBytes)
import Data.WAVE

-- =========================
import General
import MidiStuff
import Songs
import Oscillators
import Parameterised
import Steppable
import Envelopes
import Filters
import Voices
import Synths
import Scales
import Helpers
import DiatonicSequencer
import MidiSequencer
import Wavetable

import Debug.Trace


-- #TODO interpret more midi messages:
--    TempoChange and TimeDiv to get the speed of songs right
--    ProgramChange to get different instruments?
--    Velocity in NoteOn and NoteOff
-- #TODO percussion sounds - from noise oscillators
-- #TODO voices with samples - load up a general midi soundfont and use that
-- #TODO more filters:
--    Reverb
--    lowPass with resonance
--    highPass with resonance
--    bandPass
--    phaser
-- #TODO filter from laplace transform or something


-- #TODO live stream mode, plug a midi controller

-- ================================================================


x_squared :: [Float]
x_squared = map ((\x -> x*x) . (/256)) [0..256]

-- ================================================================

outputFile :: FilePath
outputFile = "output.bin"


-- =====================================================
-- =====================================================

main = do

  let midiFile = "c_major.mid"
  -- let midiFile = "onabots_2.mid"
  -- printMidi midiFile
  putStrLn $ printf $ "Playing " ++ midiFile
  play midiFile
  fail "done"



  wav <- getWAVEFile "ESW_FM_Grizzly.wav"
  putStrLn $ waveFileDescription wav

  let samples = samplesFromWave wav
  
  let synth = simpleSynth sawOsc

  let midiFile = "c_major.mid"
  putStrLn $ printf $ "Playing " ++ midiFile

  pulses <- synthesiseMidi (const synth) <$> getMidi midiFile
  saveAndPlaySound pulses

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

saveAndPlaySound :: [Pulse] -> IO ()
saveAndPlaySound pulses = do
  save $ byteStringFromPulses pulses
  -- runCommand :: String -> IO (processHandleOrSomething)
  _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate outputFile
  return ()

printMidi :: FilePath -> IO ()
printMidi inputFile = do
  midi <- getMidi inputFile

  putStrLn $ show $ timeDiv midi
  putStrLn $ (show $ length $ tracks midi) ++ " tracks found"
  mapM (mapM putStrLn . map show) $ tracks midi
  return ()


playOnabots :: IO ()
playOnabots = do
  midi <- getMidi "onabots_2.mid"


  -- Perc notes
  -- __Closed Hi-Hat (42)
  -- __Electric Bass Drum (36)
  -- __Low Tom (45)
  -- __Splash Cymbal (55)
  -- __Acoustic Snare (38)
  -- __Chinese Cymbal (52)
  -- __Crash Cymbal 1 (49)
  -- __Low-Mid Tom (47)
  -- __High Floor Tom (43)
  -- __Low Floor Tom (41)

  putStrLn $ show $ timeDiv midi
  putStrLn $ show $ length $ tracks midi


  let instruments = Map.fromList  [ (102, simpleSynth sawOsc)
                              , (80, simpleSynth sineOsc)
                              , (81, simpleSynth sawOsc)
                              , (83, simpleSynth squareOsc)
                              , (16, simpleSynth sineOsc)
                              , (33, simpleSynth squareOsc)
                              , (0, simpleSynth sawOsc)
                              ]

  let getSynth n = Map.findWithDefault (simpleSynth sineOsc) n instruments
  let pulses = synthesiseMidi getSynth midi 

  saveAndPlaySound pulses

play :: FilePath -> IO ()
play inputFile = do
  midi <- getMidi inputFile
  let pulses = synthesiseMidi (const $ AnySynth defaultSynth) midi
  saveAndPlaySound pulses



-- to play:
--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3