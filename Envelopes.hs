module Envelopes where

import General (Envelope, Seconds, Pulse, sampleRate)

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
