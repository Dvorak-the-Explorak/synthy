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

main = playOnabots


test = do

  let midiFile = "c_major.mid"

  -- wav <- getWAVEFile "bass_drum.wav"
  -- wav <- getWAVEFile "hi_hat.wav"

  nope <- samplesFromWave <$> getWAVEFile "nope.wav"
  samples <- map samplesFromWave <$> mapM getWAVEFile ["bass_drum.wav", "snare.wav", "hi_hat.wav", "low_tom.wav", "low-mid_tom.wav"]
  let sampleMapping = Map.fromList $ zip [36, 38, 42, 45, 47] samples

  let percVoice = SampledVoice (\n -> Map.findWithDefault nope n sampleMapping) (makeOneshot nope sampleRate)

  let synth = AnySynth $ defaultSynth 
                { _synthVoices = Map.empty
                , _synthVoiceTemplate = percVoice 
                }

  midi <- getMidi midiFile
  let pulses = synthesiseMidi (const synth) midi
  putStrLn $ printf $ "Playing " ++ midiFile
  saveAndPlaySound pulses

oldMain = do

  wav <- getWAVEFile "ESW_FM_Grizzly.wav"
  putStrLn $ waveFileDescription wav

  let samples = samplesFromWave wav
  

  wav <- getWAVEFile "ESW_FM_Grizzly.wav"

  let midiFile = "c_major.mid"
  putStrLn $ printf $ "Playing " ++ midiFile

  let synth = simpleSynth sawOsc

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


  -- Drum voice
  nope <- samplesFromWave <$> getWAVEFile "nope.wav"
  samples <- map samplesFromWave <$> mapM getWAVEFile ["bass_drum.wav", "snare.wav", "low_floor_tom.wav", "hi_hat.wav", "high_floor_tom.wav", "low_tom.wav", "low-mid_tom.wav", "crash.wav", "china.wav", "splash.wav"]
  let sampleMapping = Map.fromList $ zip [36, 38, 41, 42, 43, 45, 47, 49, 52, 55] samples

  let drumVoice = SampledVoice (\n -> Map.findWithDefault nope n sampleMapping) (makeOneshot nope sampleRate)
  let drumSynth = AnySynth $ defaultSynth 
                { _synthVoices = Map.empty
                , _synthVoiceTemplate = drumVoice 
                }


  let instruments = Map.fromList  [ (103, simpleSynth sawOsc)
                                  , (80, simpleSynth sineOsc)
                                  , (81, simpleSynth sawOsc)
                                  , (83, simpleSynth squareOsc)
                                  , (16, simpleSynth sineOsc)
                                  , (33, simpleSynth squareOsc)
                                  , (0, drumSynth)
                                  ]

  let getSynth n = Map.findWithDefault (simpleSynth sineOsc) n instruments
  let pulses = synthesiseMidi getSynth midi 

  saveAndPlaySound pulses

play :: FilePath -> IO ()
play inputFile = do
  midi <- getMidi inputFile
  let pulses = synthesiseMidi (const $ AnySynth defaultSynth) midi
  saveAndPlaySound pulses



-- convert to mp3 in command line:
--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3