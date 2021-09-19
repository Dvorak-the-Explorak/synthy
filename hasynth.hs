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


-- =================================================================================
-- =================================================================================
-- =================================================================================
-- =================================================================================


-- #TODO this probably doesn't all need to be in the state monad
--      Lens might be better...


type Volume = Float


type VolumeEnvelope = Either ([Volume], [Volume]) [Volume]
type FrequencyEnvelope = ([Hz], [Hz])

type OscState = (Waveform, Phase, Hz)
type VoiceState = (OscState, VolumeEnvelope)

stepOscillator :: Seconds -> State OscState Pulse
stepOscillator dt = state $ \(wave, phase, hz) -> (wave $ phase + dt*hz, (wave, phase + dt*hz, hz))

stepEnvelope :: Seconds -> State VolumeEnvelope Volume
stepEnvelope dt = state $ \venv -> case venv of
    Left ([v], decay) -> (v, Left ([v], decay))
    Left (v:vs, decay) -> (v, Left (vs, decay))
    Left ([], decay) -> (1.0, Left ([], decay))
    Right (d:ds) -> (d, Right ds)
    Right ([]) -> (0.0, Right [])


noteOffEnv :: State VolumeEnvelope ()
noteOffEnv = state $ \venv -> case venv of
    Left (_, decay) -> ((), Right decay)
    Right decay -> ((), Right decay)




-- stepVoice :: Seconds -> State VoiceState Pulse
-- stepVoice dt = state $ \(osc, vol) -> let
--     (val, osc') = runState (stepOscillator dt) osc
--     (v, vol') = runState (stepEnvelope dt) vol
--     in (v*val, (osc', vol'))

stepVoice :: Seconds -> State VoiceState Pulse
stepVoice dt = pairStatesWith (*) (stepOscillator dt) (stepEnvelope dt)

voiceOff :: State VoiceState ()
voiceOff = state $ \(osc, vol) -> ((), (osc, snd $ runState noteOffEnv vol))



-- =================================================================================
-- =================================================================================
    
-- what a F** mess

data EnvSegment = EnvAttack | EnvDecay | EnvSustain | EnvRelease | EnvDone
  deriving (Eq, Ord, Enum)
data VolEnv1 = VolEnv1 {
        attackSlope :: Float,
        decaySlope :: Float,
        sustainLevel :: Float,
        releaseSlope :: Float,
        currentState :: EnvSegment,
        volume :: Volume
    }
    
data OscState1 = OscState1 {
    wave :: Waveform,
    phase :: Phase,
    freq :: Hz
}

-- #TODO a voice should be allowed to mix multiple oscillators... 
type VoiceState1 = (OscState1, VolEnv1)

-- #TODO should maybe be called an instrument 
type SynthState1 = ([VoiceState1], [NoteNumber]) 


-- env :: Envelope
-- env = adsr (sq/8) (sq/2) 0.6 (sq/10)
-- sq = 60.0/tempo/4.0
-- #TODO un-hardcode this
voiceFromNote :: NoteNumber -> VoiceState1
voiceFromNote note = first (\osc -> osc{freq = hzFromNoteNumber note}) defaultVoice

defaultVoice :: VoiceState1
defaultVoice = let sq = 60.0/tempo/4.0
                     in (OscState1 {wave=squareTone, phase=0.0, freq=1.0},
                        VolEnv1 {attackSlope=sq/8, decaySlope=sq/2, sustainLevel=0.6, releaseSlope=sq/8,
                                currentState=EnvAttack, volume=0})

stepOsc1 :: Seconds -> State OscState1 Pulse
-- stepOsc1 dt = state $ \(wave, phase, hz) -> (wave $ phase + dt*hz, (wave, phase + dt*hz, hz))
stepOsc1 dt = state $ \osc -> let newPhase = phase osc + dt*(freq osc)
                                in (wave osc $ newPhase, osc {phase = newPhase})

restartEnv1 :: VolEnv1 -> VolEnv1
restartEnv1 venv = venv { currentState = EnvAttack}

restartEnvVoice1 :: VoiceState1 -> VoiceState1
restartEnvVoice1 = second restartEnv1


-- This is super messy :(
stepEnv1 :: Seconds -> State VolEnv1 Volume
stepEnv1 dt = state $ \venv -> case currentState venv of
  -- #TODO what if attackSlope is 0
  EnvAttack ->  let nextVol = volume venv + (attackSlope venv) * dt 
                    overStep = dt - (1 - volume venv) / (attackSlope venv)
                in if nextVol <= 1.0 
                    then (nextVol,     venv {volume = nextVol}) 
                    else runState (stepEnv1 overStep) (venv { volume=1.0, currentState = EnvDecay})
  EnvDecay -> let nextVol = volume venv - (decaySlope venv) * dt 
                  overStep = dt - (volume venv - sustainLevel venv)/(decaySlope venv)
              in if nextVol >= (sustainLevel venv)
                  then (nextVol, venv {volume = nextVol})
                  else runState (stepEnv1 overStep) (venv { volume=sustainLevel venv, currentState = EnvSustain})        
  EnvSustain -> (volume venv, venv) -- sustain, nothing changes
  EnvRelease -> let nextVol = volume venv - (releaseSlope venv) * dt 
                    overStep = dt - (volume venv) / (releaseSlope venv)
                in if nextVol >= 0
                    then (nextVol, venv {volume = nextVol})
                    else (0.0, venv {currentState = EnvDone})
  EnvDone -> (0, venv) -- stopped, ready for GC



-- jump to decay section of envelope
noteOffEnv1 :: VolEnv1 -> VolEnv1
noteOffEnv1 venv = venv {currentState=EnvRelease}
-- state can be made with:
-- noteOffEnv1 :: State VolEnv1 ()
-- noteOffEnv1 = modify noteOffEnv1

stepVoice1 :: Seconds -> State VoiceState1 Pulse
stepVoice1 dt = pairStatesWith (*) (stepOsc1 dt) (stepEnv1 dt)

-- #TODO should voice have NoteNumber, so this takes note number and does nothign if they don't match?
noteOffVoice1 :: State VoiceState1 ()
noteOffVoice1 = pairStatesWith const idState (modify noteOffEnv1)


-- const :: b -> a -> b
-- flip const :: a -> b -> b
-- -- for synth :: [voice]
-- stepSynth1 :: State SynthState1 Pulse
-- -- stepSynth1 = state $ \synth ->  let (pulses, states) = stateMap stepVoice1 synth
-- --                                     states' = filter ((<4) . currentState . second) states
-- --                                 in (sum pulses, states')
-- stepSynth1 = fmap sum $ stateMap stepVoice1

-- for synth :: ([voice], [notenumber])
stepSynth1 :: Seconds -> State SynthState1 Pulse
stepSynth1 dt = state $ \(voices, notes) ->  
    let (pulses, steppedVoices) = runState (stateMap $ stepVoice1 dt) voices
        -- running :: (VoiceState1, NoteNumber) -> Boolean
        -- running :: ((OscState1, VolEnv1), NoteNumber) -> Boolean
        running = ((<EnvDone) . currentState . snd . fst) 
        states' = unzip $ filter running $ zip steppedVoices notes
    in (sum pulses, states')

-- #TODO how do I keep track of which voices go with which notes?  
--  instead of [Voice] have map MidiNote -> Voice?
--  [Voice] and [MidiNote]?
noteOnSynth1 :: NoteNumber -> State SynthState1 ()
-- if there are any voices for that note, set the envelope to state 0
--  otherwise add a new voice for the note
noteOnSynth1 note = modify $ \(voices, notes) -> 
    let states :: [(VoiceState1, NoteNumber)]
        states = zip voices notes
    in if any (== note) notes
        -- revert envelope to state 1
        then unzip $ mapWhere ((==note) . snd) (first (second  restartEnv1)) states 
        -- add a new voice for that note
        else ((voiceFromNote note):voices, note:notes)
    

-- first :: (a -> c) -> (a,b) -> (c,b)
-- second :: (b -> c) -> (a,b) -> (a,c)

-- first . second :: (b -> d) -> ((a,b), c) -> ((a,d), c)


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
-- output = performSequence defaultSynth dorian tonalCenter jump
-- output = performSequence defaultSynth ionian tonalCenter silentNight
-- output = jumpTour
-- output = jumpTour2
-- output = silentNightFull locrian
output = silentNightFull thiccIonian
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