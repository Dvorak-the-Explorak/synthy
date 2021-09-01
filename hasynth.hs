import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable -- not just mconcat?
import System.Process
import Text.Printf
import Data.List
import Data.Fixed

-- play with:
-- ffplay -f f32le -ar 48000 output.bin

type Seconds = Float
type SamplesPerSecond = Float
type Hz = Float
type Pulse = Float
type ScaleDegree = Int
type Beats = Float
type DutyCycle = Float
type Synth = Hz -> Seconds -> [Pulse]
type Oscillator = Seconds -> Pulse
type SynthGenerator = Oscillator -> Synth


data Note = Note {
    name :: Int,
    octave :: Int
}



instance Show Note where
    show (Note n oct) | n >=0 && n < 12 = (noteNames !! n) ++ (show oct)
                      | otherwise = (noteNames !! (n `mod` 12)) ++ (show $ oct + n `div` 12)

-- enharmonic spellings ignored
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


main = do
    play
    putStrLn $ "made " ++ filename


filename :: FilePath
filename = "output.bin"

sampleRate :: SamplesPerSecond
sampleRate = 48000.0

tempo :: Beats
tempo = 100.0

semitone :: Hz
semitone = 2.0 ** (1.0/12)


-- sailorsHornpipe :: [ScaleDegree]
-- sailorsHornpipe = map (flip (-) 1) [8, 7, 8, 8, 1, 1, 1, 1, 5, 4, 3, 5, 8, 8, 8, 10, 9, 8, 9, 9, 2, 2, 2, 9, 9, 8, 7, 7, 5, 5, 5, 5]

-- sailorsHornpipe2 :: [(ScaleDegree, Seconds)]
-- sailorsHornpipe2 = map (\(x,y) -> (x-1,y)) [(8, 1), (7, 1), (8,2), (1, 2), (1, 2), (5, 1), (4, 1), (3, 1), (5, 1), (8, 2), (8, 1), (10, 1), (9, 1), (8, 1), (9, 2), (2, 2), (2, 1), (9, 1), (9, 1), (8, 1), (7, 2), (5, 2), (5, 2)]

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
        semis = map (\n -> (Just n, 1))


scale :: [ScaleDegree] -> ScaleDegree -> Hz
scale gaps n | n < 0 = 1.0/(scale (reverse gaps) (-n))
             | n == 0 = 1.0
             | n < 7 = semitone ** (fromIntegral (scanl1 (+) gaps !! (n-1)))
             | otherwise = 2 * (scale gaps $ n-7)

majorScale = scale [2, 2, 1, 2, 2, 2, 1]
minorScale = scale [2, 1, 2, 2, 2, 1, 2]

-- majorScale :: ScaleDegree -> Hz
-- majorScale n | n < 0 = 1.0/(majorScale (-n))
--              | n == 0 = 1.0
--              | n < 7 = semitone ** (scanl1 (+) gaps !! (n-1))
--              | otherwise = 2 * (majorScale $ n-7)
--     where
--         gaps = [2, 2, 1, 2, 2, 2]


synth :: Synth
synth = makePulsedSynth 0.8 pureTone



pureSynth :: Synth
pureSynth = makeSynth pureTone
sawSynth :: Synth
sawSynth = makeSynth sawTone
squareSynth :: Synth
squareSynth = makeSynth $ pwTone 0.5

pureTone :: Oscillator
pureTone = (sin . (*) (2*pi))
sawTone :: Oscillator
sawTone = (flip mod' 1.0)


pulsed :: DutyCycle -> SynthGenerator -> SynthGenerator
pulsed duty synthGen = synthGen . mult (pwTone duty)
    where
        mult f g x = (f x) * (g x)

makePulsedSynth :: DutyCycle -> SynthGenerator
makePulsedSynth duty = pulsed duty makeSynth

makePWMSynth :: Oscillator -> SynthGenerator
makePWMSynth lfo = 

pwTone :: DutyCycle -> Oscillator
pwTone duty = (\x -> if (x `mod'` 1.0 < duty) then 1.0 else 0.0)

-- take frequency, duration 
-- return samples
makeSynth :: SynthGenerator
makeSynth baseWave  = \toneFreq duration -> map (\t ->  volume * baseWave( t * toneFreq/sampleRate)) [0.0 .. sampleRate*duration]
        where 
            volume = 0.5


makeSong :: Hz -> [(Maybe ScaleDegree, Seconds)] -> [Pulse]
makeSong noteFreq notes = concat $ map (uncurry makeNote) notes
    where
        makeNote (Just n) d = articulation $ synth (noteFreq * (majorScale n)) (sq_duration * d)
        makeNote Nothing d = replicate (floor $ sq_duration * d * sampleRate) 0.0
        articulation = ar_envelope 0.02
        sq_duration = 60.0/tempo/4.0


wave :: [Pulse]
wave = wave_sailor

wave_sailor :: [Pulse]
wave_sailor = makeSong 440.0 sailorsHornpipe


wave_major :: [Pulse]
wave_major = concat [synth (hz * (majorScale i)) duration | i <- [0..14]]
    where
        hz = 440.0/2
        duration = 0.25

wave_chromatic :: [Pulse]
wave_chromatic = concat [synth (hz * semitone ** i) duration | i <- [1..12]]
    where
        hz = 440.0
        duration = 0.5

ar_envelope :: Seconds -> [Pulse] -> [Pulse]
ar_envelope slope input = zipWith (*) envelope input
    where
        rampUp = map (min 1.0) $ iterate (+stepUp) 0.0
        rampDown = reverse $ take (length input) rampUp
        envelope = zipWith (*) rampUp rampDown

        stepUp = 1.0/(slope * sampleRate)

-- B.floatLE is float little endian

song = B.toLazyByteString $ mconcat $ map B.floatLE wave


save :: IO()
save = saveAs filename

saveAs :: FilePath -> IO()
saveAs path = B.writeFile path song

play :: IO()
play = do
    save
    _ <- runCommand $ printf "ffplay  -showmode 1 -f f32le -ar %f %s" sampleRate filename
    return ()