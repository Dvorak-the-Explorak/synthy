import Prelude hiding (unzip)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import Text.Printf
import Data.List hiding (unzip)
import Data.Fixed
import Data.Tuple.Extra
import Control.Monad.State
import Control.Functor.HT (unzip)
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
freqFromPitch (Pitch pc oct) = let 
        oct' = fromIntegral oct
        pc' = fromIntegral pc
        aRef = (freqFromPitch $ Pitch 9 4)
    in 2.0**(oct'-4 + (pc'-9)/12.0) * aRef
                        

-- #TODO Reference pitch is a bit nonsense when not in 12TET
freqFromPitchTET :: Int -> Pitch -> Hz
freqFromPitchTET _ (Pitch 9 4) = 440.0
freqFromPitchTET temperament (Pitch pc oct) = let 
        oct' = fromIntegral oct
        temp' = fromIntegral temperament
        pc' = fromIntegral pc
        aRef = freqFromPitchTET temperament $ Pitch 9 4
    in 2.0** (oct'-4.0 + (pc'-9)/temp') * aRef

-- ================================================================
-- ================================================================

filename :: FilePath
filename = "output.bin"

tempo :: Beats
tempo = 88.0

tonalCenter :: Pitch
tonalCenter = Pitch 9 4

-- ================================

addSounds :: [Pulse] -> [Pulse] -> [Pulse]
addSounds = zipWith (+)

-- ==================================================================================


-- =====================================================
-- =====================================================

performSequence :: Hz -> Synth -> Scale -> Sequence -> [Pulse]
performSequence = performSequenceTET

performSequenceTET :: Hz -> Synth -> Scale -> Sequence -> [Pulse]
performSequenceTET keyHz synth scale notes = concat $ map makeChord notes
    where
        makeChord :: ([ScaleDegree], Seconds) -> [Pulse]
        makeChord ([], d) = replicate (floor $ sq * d * sampleRate) 0.0
        makeChord (ns, d) =  map ((/(fromIntegral $ length ns)) . sum) $ transpose $ map (flip makeNote d) ns
        -- makeChord (ns, d) = let 
        --                         sounds = map (\n -> makeNote n d) ns
        --                         combined [] = []
        --                         combined xs = (sum $ map head xs):combined (map tail xs)
        --                     in combined sounds

        makeNote n d = env 1.0 (sq*d) $ synth (keyHz * (scale n)) (sq * d)


        env :: Envelope
        env = adsr (sq/8) (sq/2) 0.6 (sq/10)
        sq = 60.0/tempo/4.0


-- ==========================================================================
type Volume = Float

-- #TODO continue cleaning up this mess
    
data Oscillator = Oscillator {
    wave :: Waveform,
    phase :: Phase,
    freq :: Hz
}
data EnvSegment = EnvAttack | EnvDecay | EnvSustain | EnvRelease | EnvDone
  deriving (Eq, Ord, Enum)
data VolEnv = VolEnv {
        attackSlope :: Float,
        decaySlope :: Float,
        sustainLevel :: Float,
        releaseSlope :: Float,
        currentState :: EnvSegment,
        volume :: Volume
    }

-- #TODO a voice should be allowed to mix multiple oscillators... 
--  constructing a VoicedSynth should take a voice constructor :: Note -> Voice
--  then we could replace Oscillator with [Oscillator]
type Voice = (Oscillator, VolEnv)

-- #TODO should maybe be called an instrument 
--  VoicedSynth should include LFOs
 -- should VoicedSynth include current time?
type VoicedSynth = ([Voice], [NoteNumber]) 


-- env :: Envelope
-- env = adsr (sq/8) (sq/2) 0.6 (sq/10)
-- sq = 60.0/tempo/4.0
-- #TODO un-hardcode the oscillator type and ADSR values
voiceFromNote :: NoteNumber -> Voice
voiceFromNote note = first (\osc -> osc{freq = hzFromNoteNumber note}) defaultVoice

defaultVoice :: Voice
defaultVoice = (Oscillator {wave=sawTone, phase=0.0, freq=1.0},
                VolEnv {attackSlope=2, decaySlope=1, sustainLevel=0.6, releaseSlope=1,
                        currentState=EnvAttack, volume=0})

stepOsc :: Seconds -> State Oscillator Pulse
-- stepOsc dt = state $ \(wave, phase, hz) -> (wave $ phase + dt*hz, (wave, phase + dt*hz, hz))
stepOsc dt = state $ \osc -> let newPhase = flip mod' 1.0 $ phase osc + dt*(freq osc)
                                in (wave osc $ newPhase, osc {phase = newPhase})


restartEnv :: VolEnv -> VolEnv
restartEnv venv = venv { currentState = EnvAttack}

-- jump to decay section of envelope
noteOffEnv :: VolEnv -> VolEnv
noteOffEnv venv = venv {currentState=EnvRelease}


-- This is super messy :(
stepEnv :: Seconds -> State VolEnv Volume
stepEnv dt = state $ \venv -> case currentState venv of
  -- #TODO what if attackSlope is 0
  EnvAttack ->  let nextVol = volume venv + (attackSlope venv) * dt 
                    overStep = dt - (1 - volume venv) / (attackSlope venv)
                in if nextVol <= 1.0 
                    then (nextVol,     venv {volume = nextVol}) 
                    else runState (stepEnv overStep) (venv { volume=1.0, currentState = EnvDecay})
  EnvDecay -> let nextVol = volume venv - (decaySlope venv) * dt 
                  overStep = dt - (volume venv - sustainLevel venv)/(decaySlope venv)
              in if nextVol >= (sustainLevel venv)
                  then (nextVol, venv {volume = nextVol})
                  else runState (stepEnv overStep) $
                        (venv { volume=sustainLevel venv, currentState = EnvSustain})        
  EnvSustain -> (volume venv, venv) -- sustain, nothing changes
  EnvRelease -> let nextVol = (volume venv) - (releaseSlope venv) * dt 
                in if nextVol >= 0
                    then (nextVol, venv {volume = nextVol})
                    else (0.0, venv {currentState = EnvDone})
  EnvDone -> (0, venv) -- stopped, ready for GC





stepVoice :: Seconds -> State Voice Pulse
stepVoice dt = pairStatesWith (*) (stepOsc dt) (stepEnv dt)


-- #TODO should voice have NoteNumber, so this takes note number and does nothign if they don't match?
-- noteOffVoice1 :: State Voice ()
-- noteOffVoice1 = pairStatesWith const idState (modify noteOffEnv)
-- noteOffVoice1 :: Voice -> Voice
-- noteOffVoice1 = second noteOffEnv
-- SHOLDN'T be called noteOff when there's no reference to NoteNuber in the Voice type
releaseVoice :: Voice -> Voice
releaseVoice = second noteOffEnv

-- restartVoice :: State Voice ()
-- restartVoice = pairStatesWith const idState (modify restartEnv)
restartVoice :: Voice -> Voice
restartVoice = second restartEnv


-- for synth :: ([voice], [notenumber])

-- #TODO is it okay to just sym the synths?  Could do some sort of compression on output
stepSynth :: Seconds -> State VoicedSynth Pulse
stepSynth dt = state $ \(voices, notes) ->  
    let (pulses, steppedVoices) = runState (stateMap $ stepVoice dt) voices
        -- running :: (Voice, NoteNumber) -> Boolean
        -- running :: ((Oscillator, VolEnv), NoteNumber) -> Boolean
        running = ((<EnvDone) . currentState . snd . fst) 
        states' = unzip $ filter running $ zip steppedVoices notes
    in (sum pulses, states')

-- #TODO is ([Voice], [MidiNote]) the best way to keep track of which note each voice corresponds?
--          need to know in order to handle NoteOff        
--  instead of [Voice] have map MidiNote -> Voice?
--  Voice could include its MidiNote?

restartVoices :: NoteNumber -> ([Voice], [NoteNumber]) -> ([Voice], [NoteNumber])
restartVoices note (voices, notes) = unzip $ mapWhere 
                                              ((==note) . snd) 
                                              (first restartVoice) 
                                              $ zip voices notes
releaseVoices :: NoteNumber -> ([Voice], [NoteNumber]) -> ([Voice], [NoteNumber])
releaseVoices note (voices, notes) = unzip $ mapWhere 
                                              ((==note) . snd) 
                                              (first releaseVoice) 
                                              $ zip voices notes

-- if there are any voices for that note, set the envelope to EnvAttack state
--  otherwise add a new voice for the note
noteOnSynth :: NoteNumber -> State VoicedSynth ()
noteOnSynth note = modify $ \(voices, notes) -> 
    if any (== note) notes
      -- revert envelope to state 1
      then restartVoices note (voices, notes)
      -- add a new voice for that note
      else ((voiceFromNote note):voices, note:notes)

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffSynth :: NoteNumber -> State VoicedSynth ()
noteOffSynth note = modify $ releaseVoices note 


-- #TODO give voices a "run" function 
--      so this doesn't have to step all the states for each sample
-- #TODO handle midi timing with fractional samples
runSynth :: Seconds -> State VoicedSynth [Pulse]
runSynth dt | dt < (1.0/sampleRate) = return []
             | otherwise = do 
                  pulse <- stepSynth (1.0/sampleRate)
                  pulses <- runSynth (dt - (1.0/sampleRate))
                  return (pulse:pulses)


data ToyMidi = ToyNoteOn NoteNumber Seconds | ToyNoteOff NoteNumber Seconds | ToyNothing Seconds


synthesiseMidi :: [ToyMidi] -> State VoicedSynth [Pulse]
synthesiseMidi [] = return []
synthesiseMidi ((ToyNoteOn note dt):mids) = do 
  output <- runSynth dt
  noteOnSynth note
  remainder <- synthesiseMidi mids
  return $ output ++ remainder
synthesiseMidi ((ToyNoteOff note dt):mids) = do
    output <- runSynth dt
    noteOffSynth note
    remainder <- synthesiseMidi mids
    return $ output ++ remainder
synthesiseMidi ((ToyNothing dt):mids) = do
    output <- runSynth dt
    remainder <- synthesiseMidi mids
    return $ output ++ remainder

testSeq1 :: [ToyMidi]
testSeq1 = [ToyNoteOn 69 0, ToyNoteOff 69 5,
            ToyNothing 5]

testSeq2 :: [ToyMidi]
testSeq2 = [ToyNoteOn 69 0, ToyNoteOn 73 0.5, ToyNoteOn 76 0.5, ToyNoteOn 81 0.5, 
            ToyNoteOff 69 2, ToyNoteOff 73 0, ToyNoteOff 76 0, ToyNoteOff 81 0,
            ToyNothing 5]


-- =====================================================
-- =====================================================

main = do
    putStrLn $ printf "Playing song in %s" $ show tonalCenter 
    play
    -- printSong
    putStrLn $ "made " ++ filename

-- B.floatLE is float little endian
song = B.toLazyByteString $ mconcat $ map B.floatLE output

printSong :: IO ()
printSong = putStrLn $ concatMap ((++" ") . show) $ zip scaleDegrees freqs
    where
        scaleDegrees = [-7..21]
        freqs = map ionian19TET scaleDegrees

output :: [Pulse]
output = fst $ runState (synthesiseMidi testSeq2) ([], [])
-- output = silentNightFull thiccIonian
-- output = performSequence defaultSynth dorian tonalCenter jump
-- output = performSequence defaultSynth ionian tonalCenter silentNight
-- output = jumpTour
-- output = jumpTour2
-- output = silentNightFull locrian
-- output = jumpFull phrygian
-- output = performSequence pureSynth ionian tonalCenter silentNightChords


silentNightFull :: Scale -> [Pulse]
silentNightFull modality = map (/3) $ addSounds bassline $ addSounds melody chords 
    where
        chords = map (*2) $ performSequence (freqFromPitch tonalCenter) pureSynth modality silentNightChords
        melody = performSequence (freqFromPitch tonalCenter) sawSynth modality silentNightMelody
        bassline = performSequence (freqFromPitch tonalCenter) squareSynth modality silentNightBassline

silentNightFullTET :: Scale -> [Pulse]
silentNightFullTET modality = map (/3) $ addSounds bassline $ addSounds melody chords 
    where
        -- #TODO reference frequency always 440, doesn't really make sense
        chords = map (*2) $ performSequenceTET (freqFromPitch tonalCenter) pureSynth modality silentNightChords
        melody = performSequenceTET (freqFromPitch tonalCenter) sawSynth modality silentNightMelody
        bassline = performSequenceTET (freqFromPitch tonalCenter) squareSynth modality silentNightBassline

jumpFull :: Scale -> [Pulse]
jumpFull modality = map (/2) $ addSounds bassline chords
    where
        bassline = performSequence (freqFromPitch tonalCenter) squareSynth modality jumpBassline
        chords = performSequence (freqFromPitch tonalCenter) sawSynth modality jump


jumpTour :: [Pulse]
jumpTour = mconcat $ map (\scale -> performSequence (freqFromPitch tonalCenter) defaultSynth scale jump) modes
    where 
        modes =  [ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian]


jumpTour2 :: [Pulse]
jumpTour2 = mconcat $ map (\scale -> performSequence (freqFromPitch tonalCenter) defaultSynth scale jump) modes
    where
        modes = [lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian]


save :: IO()
save = saveAs filename

saveAs :: FilePath -> IO()
saveAs path = B.writeFile path song

play :: IO ()
play = do
    save
    -- runCommand :: String -> IO (processHandleOrSomething)
    _ <- runCommand $ printf "ffplay -loglevel quiet -loop 1 -showmode 2 -f f32le -ar %f %s" sampleRate filename
    return ()

--ffmpeg -f f32le -ar 48000.0 -i output.bin output.mp3