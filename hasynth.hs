import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import Text.Printf
import Data.List
import Data.Fixed
import Debug.Trace

-- play with:
-- ffplay -f f32le -ar 48000 output.bin

type Seconds = Float
type SamplesPerSecond = Float
type Hz = Float
type Pulse = Float
type ScaleDegree = Int
type Beats = Float
type DutyCycle = Float
type Velocity = Float
type Synth = Hz -> Seconds -> [Pulse]
type Oscillator = Seconds -> Pulse
type SynthGenerator = Oscillator -> Synth
type Scale = ScaleDegree -> Hz
-- Kinda don't like that the envelope acts on samples
type Envelope = Velocity -> Seconds -> [Pulse] -> [Pulse]


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
-- ================================================================
-- ================================================================

filename :: FilePath
filename = "output.bin"

sampleRate :: SamplesPerSecond
sampleRate = 48000.0

tempo :: Beats
tempo = 100.0

tonalCenter :: Pitch
tonalCenter = Pitch 0 4

semitone :: Hz
semitone = 2.0 ** (1.0/12)


sailorsHornpipe :: [(Maybe ScaleDegree, Seconds)]
sailorsHornpipe = map (\(x,y) -> (fmap (\z -> z-1) x,y)) $ [(Just 8, 1), (Just 7, 1), 
                                            (Just 8, 1), (Nothing, 1), (Just 1, 1), (Nothing, 1), (Just 1, 2), (Just 5, 1), (Just 4, 1), 
                                            (Just 3, 1), (Just 5, 1), (Just 8, 1), (Nothing, 1), (Just 8, 1), (Just 10, 1), (Just 9, 1), (Just 8, 1), 
                                            (Just 9, 1), (Nothing, 1), (Just 2, 1), (Nothing, 1), (Just 2, 1), (Just 9, 1), (Just 9, 1), (Just 8, 1), 
                                            (Just 7, 2), (Just 5, 1), (Nothing, 1), (Just 5, 2)] ++ semis [6, 7, 
                                            8, 7, 6, 5, 6, 5, 4, 3, 
                                            4, 3, 2, 1, 2, 1, 0, -2,
                                            -1, 1, 0, 2, 1, 3, 2, 4, 
                                            3] ++ [(Nothing, 1), (Just 1, 1), (Nothing, 1), (Just 1, 1), (Nothing, 1)]
    where 
        -- just make a list of notes with given pitches and duration of 1
        semis = map (\n -> (Just n, 1))

-- =======================================================================

-- more like [Interval] -> Scale
scaleFromIntervals :: [ScaleDegree] -> Scale
scaleFromIntervals gaps n | n < 0 = 1.0/(scaleFromIntervals (reverse gaps) (-n))
             | n == 0 = 1.0
             | n < 7 = semitone ** (fromIntegral (scanl1 (+) gaps !! (n-1)))
             | otherwise = 2 * (scaleFromIntervals gaps $ n-7)

majorScale = ionian
minorScale = dorian
ionian = scaleFromIntervals [2, 2, 1, 2, 2, 2, 1]
dorian = scaleFromIntervals $ rot 1 [2, 2, 1, 2, 2, 2, 1]
phrygian = scaleFromIntervals $ rot 2 [2, 2, 1, 2, 2, 2, 1]
lydian = scaleFromIntervals $ rot 3 [2, 2, 1, 2, 2, 2, 1]
mixolydian = scaleFromIntervals $ rot 4 [2, 2, 1, 2, 2, 2, 1]
aeolian = scaleFromIntervals $ rot 5 [2, 2, 1, 2, 2, 2, 1]
locrian = scaleFromIntervals $ rot 6 [2, 2, 1, 2, 2, 2, 1]

-- ==================================================================================

synth :: Synth
-- synth = makePulsedSynth 0.8 pureTone
synth = sawSynth
-- synth = pureSynth

pureSynth :: Synth
pureSynth = makeSynth pureTone
sawSynth :: Synth
sawSynth = makeSynth sawTone
squareSynth :: Synth
squareSynth = makeSynth squareTone

pureTone :: Oscillator
pureTone = (sin . (*) (2*pi))

sawTone :: Oscillator
-- sawTone = (flip (-) 1) . (*2) . (flip mod' 1.0)
sawTone = \t -> 2 * (t `mod'` 1) - 1

squareTone :: Oscillator
squareTone = (\t -> if (t `mod'` 1.0 < 0.5) then -1.0 else -1.0)

-- synced to start of note only, no globally timed LFO
makePWMSynth :: Oscillator -> SynthGenerator
makePWMSynth lfo = makeSynth . mult modulator
    where
        mult f g x = (f x) * (g x)
        modulator = (\x -> if (x `mod'` 1.0 < lfo x) then 1.0 else 0.0)

-- take frequency, duration 
-- return samples
makeSynth :: SynthGenerator
makeSynth baseWave  = \toneFreq duration -> map (\t ->  volume * baseWave( t * toneFreq/sampleRate)) [0.0 .. sampleRate*duration]
        where 
            volume = 0.5


-- =====================================================================
-- =====================================================================s



-- ramp up/down time -> input -> output
ar_envelope :: Seconds -> [Pulse] -> [Pulse]
ar_envelope rampTime input = zipWith (*) envelope input
    where
        rampUp = map (min 1.0) $ iterate (+stepUp) 0.0
        rampDown = reverse $ take (length input) rampUp
        envelope = zipWith (*) rampUp rampDown

        stepUp = 1.0/(rampTime * sampleRate)

-- Linear attack, decay and release
adsr :: Seconds -> Seconds -> Pulse -> Seconds -> Envelope
adsr attackTime decayTime susLevel releaseTime = \vel duration input -> 
    let 
        output = zipWith (*) env input

        env = attack ++ decay ++ sustain ++ release ++ repeat 0.0

        attack = if attackTime > 0 
                then map (*attackStep) [0.0 .. vel/attackStep] 
                else []
        -- #TODO does [10..1] not work???
        decay = if decayTime > 0 
                then map (*decayStep) $ reverse [vel*susLevel/decayStep .. vel/decayStep] 
                else []


        -- #TODO what if duration is less than attack + decay
        -- release won't start at sustain level
        --  test this
        decayStartLevel = if length attack + length decay < (floor $ duration*sampleRate)
                            then susLevel*vel
                            else (attack ++ decay) !! ((floor $ duration * sampleRate) - 1)

        sustain = replicate (max 0 $ (floor $ duration*sampleRate) - length attack - length decay) $ susLevel*vel
        
        release = if releaseTime > 0 
                then map (*releaseStep) $ reverse [0.0 .. vel*susLevel/releaseStep]
                else []

        attackStep = 1.0/(attackTime * sampleRate)
        decayStep = 1.0/(decayTime * sampleRate)
        releaseStep = 1.0/(releaseTime * sampleRate)
    in output


-- =====================================================

makeSong :: Scale ->  Pitch -> [(Maybe ScaleDegree, Seconds)] -> [Pulse]
makeSong scale pitch notes = concat $ map (uncurry makeNote) notes
    where
        makeNote (Just n) d = env 1.0 (sq*d) $ synth ((freqFromPitch pitch) * (scale n)) (sq * d)
        makeNote Nothing d = replicate (floor $ sq * d * sampleRate) 0.0
        env :: Envelope
        env = adsr (sq/8) (sq/2) 0.6 (sq/10)
        sq = 60.0/tempo/4.0

-- =====================================================
-- =====================================================
-- =====================================================

main = do
    putStrLn $ printf "Playing Sailor's Hornpipe in %s" $ shows tonalCenter 
    play
    putStrLn $ "made " ++ filename

-- B.floatLE is float little endian
song = B.toLazyByteString $ mconcat $ map B.floatLE wave

wave :: [Pulse]
wave = makeSong lydian tonalCenter sailorsHornpipe

save :: IO()
save = saveAs filename

saveAs :: FilePath -> IO()
saveAs path = B.writeFile path song

play :: IO()
play = do
    save
    -- runCommand :: String -> IO (processHandleOrSomething)
    _ <- runCommand $ printf "ffplay -loglevel quiet -showmode 2 -f f32le -ar %f %s" sampleRate filename
    return ()


rot :: Int -> [a] -> [a]
rot _ [] = []
rot 0 xs = xs
rot n (x:xs) = rot (n-1) $ xs ++ [x]