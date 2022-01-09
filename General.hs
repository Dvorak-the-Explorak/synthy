module General where

type Volume = Float
type Seconds = Float
type SamplesPerSecond = Float
type Hz = Float
type Phase = Float
type Pulse = Float
type Waveform = Phase -> Pulse -- pure waveform, can evaluate its pulse from just phase
type WaveIndex = Float
type ScaleDegree = Int
type Beats = Float
type DutyCycle = Float
type Velocity = Float
type Scale = ScaleDegree -> Hz
type Sequence = [([ScaleDegree], Seconds)]

addSounds :: [Pulse] -> [Pulse] -> [Pulse]
addSounds = zipWith (+)

-- #TODO make this less hardcoded
sampleRate :: SamplesPerSecond
sampleRate = 48000.0



semitone :: Hz
semitone = 2.0 ** (1.0/12)

