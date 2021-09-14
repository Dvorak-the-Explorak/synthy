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
freqFromPitch (Pitch pc oct) = 2.0**((fromIntegral oct)-4 + ((fromIntegral pc)-9)/12.0) * (freqFromPitch $ Pitch 9 4)
                        

-- #TODO Reference pitch is a bit nonsense when not in 12TET
freqFromPitchTET :: Int -> Pitch -> Hz
freqFromPitchTET _ (Pitch 9 4) = 440.0
freqFromPitchTET temperament (Pitch pc oct) = 2.0** ((fromIntegral oct)-4.0 + ((fromIntegral pc)-9)/(fromIntegral temperament)) * (freqFromPitchTET temperament $ Pitch 9 4)

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

performSequence :: Synth -> Scale ->  Pitch -> Sequence -> [Pulse]
performSequence = performSequenceTET 12

performSequenceTET :: Int -> Synth -> Scale ->  Pitch -> Sequence -> [Pulse]
performSequenceTET temperament synth scale pitch notes = concat $ map makeChord notes
    where
        keyHz = (freqFromPitchTET temperament pitch)

        makeChord :: ([ScaleDegree], Seconds) -> [Pulse]
        makeChord ([], d) = replicate (floor $ sq * d * sampleRate) 0.0
        makeChord (ns, d) =  map sum $ transpose $ map (\n -> makeNote n d) ns
        -- makeChord (ns, d) = let 
        --                         sounds = map (\n -> makeNote n d) ns
        --                         combined [] = []
        --                         combined xs = (sum $ map head xs):combined (map tail xs)
        --                     in combined sounds

        makeNote n d = env 1.0 (sq*d) $ synth (keyHz * (scale n)) (sq * d)
        env :: Envelope
        env = adsr (sq/8) (sq/2) 0.6 (sq/10)
        sq = 60.0/tempo/4.0




-- #V1 do it all at once :(
-- --          startTime  note
-- type Voice = (Seconds, NoteNumber)
-- type InstrumentState = (Seconds, [Voice])

-- -- #TODO how will we put envelopes in here
-- performMidiSequence :: ChannelNumber -> Waveform -> MidiSequence -> [Pulse]
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



-- #TODO envelopes are going to need to be able to do something when noteOff happens...
type Volume = Float
type VolumeEnvelope = Either ([Volume], [Volume]) [Volume]
type FrequencyEnvelope = ([Hz], [Hz])

-- -- #V2 inject (State Voice) into State [Voice]
-- type Voice = (Phase, Waveform, Hz, VolumeEnvelope)
-- data Voice = Voice {
--     phase :: Phase,
--     wave :: Waveform,
--     freq :: Hz,
--     vol :: VolumeEnvelope
-- }

-- stepVoice :: Seconds -> State Voice Pulse
-- stepVoice dt = state $ run
--     where
--         run voice = case vol voice of
--             Left (v:vs, [decay]) -> (v * (wave $ phase + dt * freq), voice {vol=Left (vs, decay), phase = phase+freq*dt})
--             Right v:vs -> (v * (wave $ phase + dt * freq), voice {vol=Right vs, phase = phase+freq*dt})


type OscState = (Waveform, Phase, Hz)
type VoiceState = (OscState, VolumeEnvelope)

stepOscillator :: Seconds -> State OscState Pulse
stepOscillator dt = state $ \(wave, phase, hz) -> (wave $ phase + dt*hz, (wave, phase + dt*hz, hz))

stepEnvelope :: Seconds -> State VolumeEnvelope Volume
stepEnvelope dt = state $ \venv -> case venv of
    Left (v:vs, decay) -> (v, Left (vs, decay))
    Right (d:ds) -> (d, Right ds)

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
stepVoice dt = joinStatesWith (*) (stepOscillator dt) (stepEnvelope dt)

voiceOff :: State VoiceState ()
voiceOff = state $ \(osc, vol) -> ((), (osc, snd $ runState noteOffEnv vol))


-- joinStatesWith :: State s1 a -> State s2 b -> (a -> b -> c) -> State (s1, s2) c
-- joinStatesWith op1 op2 f = state $ \(s1, s2) -> let
--     (out1, s1') = runState op1 s1
--     (out2, s2') = runState op2 s2
--     in (f out1 out2, (s1', s2'))

joinStatesWith ::  (a -> b -> c) -> State s1 a -> State s2 b -> State (s1, s2) c
joinStatesWith f op1 op2 = state $ \s -> let
    result = runState op1 *** runState op2 $ s
    out = uncurry f . (fst *** fst) $ result
    s' = snd *** snd $ result
    in (out, s')


-- from Prelude or Data.List
-- unzip :: [(a,b)] -> ([a], [b])
enlist :: State s a -> State [s] [a]
enlist op = state $ unzip . map (runState op)

-- from Data.Functor.HT
-- unzip :: Function f => f (a,b) -> (f a, f b)
injectState :: Functor f => State s a -> State (f s) (f a)
injectState op = state $ unzip . fmap (runState op)

injectWith :: Functor f => (f a -> a) -> State s a -> State (f s) a
injectWith combine op = state $ first combine . unzip . fmap (runState op)




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
-- wave = silentNightFull locrian
wave = silentNightFullTET 19 ionian19TET
-- wave = jumpFull phrygian
-- wave = performSequence pureSynth ionian tonalCenter silentNightChords


silentNightFull :: Scale -> [Pulse]
silentNightFull modality = map (/3) $ addSounds bassline $ addSounds melody chords 
    where
        chords = map (*2) $ performSequence pureSynth modality tonalCenter silentNightChords
        melody = performSequence sawSynth modality tonalCenter silentNightMelody
        bassline = performSequence squareSynth modality tonalCenter silentNightBassline

silentNightFullTET :: Int ->  Scale -> [Pulse]
silentNightFullTET temperament modality = map (/3) $ addSounds bassline $ addSounds melody chords 
    where
        chords = map (*2) $ performSequenceTET 19 pureSynth modality tonalCenter silentNightChords
        melody = performSequenceTET 19 sawSynth modality tonalCenter silentNightMelody
        bassline = performSequenceTET 19 squareSynth modality tonalCenter silentNightBassline

jumpFull :: Scale -> [Pulse]
jumpFull modality = map (/2) $ addSounds bassline chords
    where
        bassline = performSequence squareSynth modality tonalCenter jumpBassline
        chords = performSequence sawSynth modality tonalCenter jump


jumpTour :: [Pulse]
jumpTour = mconcat $ map (\scale -> performSequence defaultSynth scale tonalCenter jump) modes
    where 
        modes =  [ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian]


jumpTour2 :: [Pulse]
jumpTour2 = mconcat $ map (\scale -> performSequence defaultSynth scale tonalCenter jump) modes
    where
        modes = [lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian]


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

