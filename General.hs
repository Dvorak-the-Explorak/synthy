module General where

type Volume = Float
type Seconds = Float
type SamplesPerSecond = Float
type Hz = Float
type Phase = Float
type Pulse = Float
type ScaleDegree = Int
type Beats = Float
type DutyCycle = Float
type Velocity = Float
type Scale = ScaleDegree -> Hz
-- Kinda don't like that the envelope acts on samples
-- type Envelope = Velocity -> Seconds -> [Pulse] -> [Pulse]
type Sequence = [([ScaleDegree], Seconds)]

addSounds :: [Pulse] -> [Pulse] -> [Pulse]
addSounds = zipWith (+)

-- #TODO make this less hardcoded
sampleRate :: SamplesPerSecond
sampleRate = 48000.0

semitone :: Hz
semitone = 2.0 ** (1.0/12)

