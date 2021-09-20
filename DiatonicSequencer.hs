module DiatonicSequencer where

import Data.List (transpose)


import General
import Synths
import Scales
import Songs
import Helpers
import Envelopes

tonalCenter :: Pitch
tonalCenter = Pitch 9 4

tempo :: Beats
tempo = 88.0

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




-- ==================================================================

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



-- ====================================================================================================





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
jumpTour = mconcat $ map (\scale -> performSequence (freqFromPitch tonalCenter) sawSynth scale jump) modes
    where 
        modes =  [ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian]


jumpTour2 :: [Pulse]
jumpTour2 = mconcat $ map (\scale -> performSequence (freqFromPitch tonalCenter) sawSynth scale jump) modes
    where
        modes = [lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian]

