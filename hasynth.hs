{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
  #-}
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
import Control.Lens
import Control.Lens.Tuple
-- =========================
import General
import MidiStuff
import Songs
import Synths
import Scales
import Envelopes
import Helpers
import DiatonicSequencer
-- import Debug.Trace

-- #TODO play from MIDI file
-- #TODO percussion sounds - white noise


-- ================================================================
-- ================================================================

filename :: FilePath
filename = "output.bin"



data Oscillator = Oscillator {
  _wave :: Waveform,
  _phase :: Phase,
  _freq :: Hz
}
data EnvSegment = EnvAttack | EnvDecay | EnvSustain | EnvRelease | EnvDone
  deriving (Eq, Ord, Enum)
data VolEnv = VolEnv {
  _attackSlope :: Float,
  _decaySlope :: Float,
  _sustainLevel :: Float,
  _releaseSlope :: Float,
  _currentState :: EnvSegment,
  _volume :: Volume
}
envSlope :: VolEnv -> Float
envSlope venv = case _currentState venv of
  EnvAttack -> _attackSlope venv
  EnvDecay -> -(_decaySlope venv)
  EnvSustain -> 0.0
  EnvRelease -> -(_releaseSlope venv)
  EnvDone -> 0.0
stillInSegment :: VolEnv -> Float -> Bool
stillInSegment venv x = case _currentState venv of
  EnvAttack -> (x <= 1.0)
  EnvDecay -> (x >= (_sustainLevel venv))
  EnvSustain -> True
  EnvRelease -> (x >= 0.0)
  EnvDone -> True
-- just jump to next segment.
--  if _volume hadn't reached the threshold for the next segment, just jump to it
--    - should only be a small amount between samples
toNextSegment :: VolEnv -> VolEnv
toNextSegment venv = case _currentState venv of
  EnvAttack -> venv {_volume = 1.0, _currentState = EnvDecay}
  EnvDecay -> venv {_volume = _sustainLevel venv, _currentState = EnvSustain}
  EnvSustain -> venv {_currentState = EnvRelease}
  EnvRelease -> venv {_volume = 0.0, _currentState = EnvDone}
  EnvDone -> venv
timeToSegmentTarget :: VolEnv -> Seconds
timeToSegmentTarget venv = case _currentState venv of
  EnvAttack -> (1 - _volume venv) / (_attackSlope venv)
  EnvDecay -> (_volume venv - _sustainLevel venv)/(_decaySlope venv)
  EnvSustain -> 0.0
  EnvRelease -> (_volume venv)/(_releaseSlope venv)
  EnvDone -> 0.0
timestepWithinSegment :: VolEnv -> Seconds -> Bool
timestepWithinSegment venv dt = case _currentState venv of
  EnvAttack -> dt <= (1 - _volume venv) / (_attackSlope venv)
  EnvDecay -> dt <= (_volume venv - _sustainLevel venv)/(_decaySlope venv)
  EnvSustain -> True
  EnvRelease -> dt <= (_volume venv)/(_releaseSlope venv)
  EnvDone -> True

-- #TODO turn these `type` declarations into `data` declarations and lens it up boi
-- #TODO a voice should be allowed to mix multiple oscillators... 
--  constructing a VoicedSynth should take a voice constructor :: Note -> Voice
--  then we could replace Oscillator with [Oscillator]
data Voice = Voice {
  _voiceOsc :: Oscillator, 
  _voiceVenv :: VolEnv,
  _voiceNote :: NoteNumber
}
-- type Voice = (Oscillator, VolEnv)

-- #TODO should maybe be called an instrument / sequencer 
--  VoicedSynth should include LFOs
 -- should VoicedSynth include current time?
type VoicedSynth = ([Voice]) 

-- The state here probably doesn't need to be [Pulse]
type FilterState = [Pulse]
type Filter = Pulse -> State FilterState Pulse
-- type FilterState = (a, a -> Pulse -> (a,Pulse))
-- type FilterState = State a Pulse
type FullSynth = (VoicedSynth, FilterState, Filter)


makeLenses ''Oscillator
makeLenses ''VolEnv
makeFields ''Voice

-- env :: Envelope
-- env = adsr (sq/8) (sq/2) 0.6 (sq/10)
-- sq = 60.0/tempo/4.0
-- #TODO un-hardcode the oscillator type and ADSR values
voiceFromNote :: NoteNumber -> Voice
voiceFromNote noteNum = defaultVoice  & osc . freq .~ (hzFromNoteNumber noteNum)
                                      & note .~ noteNum
-- voiceFromNote note = set (_1.freq) (hzFromNoteNumber note) defaultVoice
-- voiceFromNote note = over _1 (\osc -> osc{_freq = hzFromNoteNumber note}) defaultVoice

defaultVoice :: Voice
defaultVoice = Voice {
    _voiceOsc = Oscillator {
        _wave=sawTone, _phase=0.0, _freq=1.0
    },
    _voiceVenv = VolEnv {
        _attackSlope=2, _decaySlope=1, _sustainLevel=0.6, 
        _releaseSlope=1, _currentState=EnvAttack, _volume=0
    },
    _voiceNote = 0
}

stepOsc :: Seconds -> State Oscillator Pulse
stepOsc dt = state $ \os -> let newPhase = flip mod' 1.0 $ os ^. phase + dt*(os ^. freq)
                              in ( (*0.1) $ os ^. wave $ newPhase, os & phase .~ newPhase)

-- should basically act like iterating stepOsc N times
runOsc :: Int -> Seconds -> State Oscillator [Pulse]
runOsc 0 dt = return []
runOsc n dt = do
  osc <- get
  let nextPhase = flip mod' 1.0 $ (osc ^. phase) + dt*(osc ^. freq)*(fromIntegral n)
  let phases = map (\i -> (dt*(osc ^. freq)*(fromIntegral i)) + osc ^. phase) [1..n]
  let outputs = map (osc ^. wave) $ phases
  put $ osc & phase .~ nextPhase
  return outputs



restartEnv :: VolEnv -> VolEnv
restartEnv venv = venv & currentState .~ EnvAttack

-- jump to decay section of envelope
noteOffEnv :: VolEnv -> VolEnv
noteOffEnv venv = venv & currentState .~ EnvRelease

stepEnv :: Seconds -> State VolEnv Volume
stepEnv dt = do
  venv <- get
  let slope = envSlope venv
  let nextVol = (dt*slope) + (venv ^. volume)
  if timestepWithinSegment venv dt
    then (put $ venv & volume .~ nextVol) >> return nextVol
         -- in state $ \venv -> (nextVol, venv {_volume = nextVol})
    else withState toNextSegment $ stepEnv (dt - timeToSegmentTarget venv)


runEnv :: Int -> Seconds -> State VolEnv [Volume]
runEnv 0 dt = return []
runEnv n dt = do
  venv <- get
  let slope = envSlope venv
  let steps = take n $ tail $ iterate (+(dt*slope)) (venv ^. volume)
  let valid = takeWhile (stillInSegment venv) steps
  nextSegmentSteps <- if length valid == n
                        then (put $ venv & volume .~ last valid) >> return []
                        -- then state $ \venv -> ([], venv {_volume = last valid})
                        else withState toNextSegment $ runEnv (n - (length valid)) dt
  return $ valid ++ nextSegmentSteps


stepVoice :: Seconds -> State Voice Pulse
stepVoice dt = joinStatesWith (*) (overState osc $ stepOsc dt) (overState venv $ stepEnv dt)
    -- stepVoice dt = pairStatesWith (*) (stepOsc dt) (stepEnv dt)

runVoice :: Int -> Seconds -> State Voice [Pulse]
runVoice n dt = joinStatesWith (zipWith (*)) (overState osc $ runOsc n dt) (overState venv $ runEnv n dt)
    -- runVoice n dt = pairStatesWith (zipWith (*)) (runOsc n dt) (runEnv n dt)


-- #TODO should voice have NoteNumber, so this takes note number and does nothign if they don't match?
releaseVoice :: Voice -> Voice
releaseVoice = over venv noteOffEnv

restartVoice :: Voice -> Voice
restartVoice = over venv restartEnv


-- (stateMap $ stepVoice dt) :: State [Voice] [Pulse]
-- firstState :: State s1 a -> State (s1,s2) a
-- firstState (stateMap $ stepVoice dt) :: State ([Voice], a) [Pulse]
-- fmap sum :: State s [a] -> State s a
-- fmap sum $ firstState (stateMap $ stepVoice dt) :: State ([Voice], a) Pulse
stepSynthVoices :: Seconds -> State VoicedSynth Pulse
stepSynthVoices dt = fmap sum $ stateMap $ stepVoice dt


-- #TODO this isn't actually running, just iterating steps
-- runVoice n dt :: State Voice [Pulse for each sample]
-- stateMap $ runVoice n dt :: State [Voice] [[Pulse foreach sample] foreach voice]
-- firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [[Pulse foreach sample] foreach voice]
-- fmap (map sum . transpose) $ firstState $ stateMap $ runVoice n dt :: State ([Voice], a)  [Pulse foreach sample (summed over voices)]
runSynthVoices :: Int -> Seconds -> State VoicedSynth [Pulse]
runSynthVoices 0 dt = return []
runSynthVoices n dt = do
  pulse <- stepSynthVoices dt
  pulses <- runSynthVoices (n-1) dt
  return (pulse:pulses)


-- running :: (Voice, NoteNumber) -> Boolean
-- running :: ((Oscillator, VolEnv), NoteNumber) -> Boolean
-- running ((osc, env), note) = _currentState env < EnvDone
cullSynthVoices :: State VoicedSynth ()
-- cullSynthVoices = let running = ((<EnvDone) . _currentState . snd . fst) 
-- cullSynthVoices = let running = ((<EnvDone) . _currentState . (view (_1 . _2)) )
cullSynthVoices = let running = views (venv . currentState) (<EnvDone)
                  in modify (filter running)

-- -- #TODO At some point there should be explicit clipping.  Should it be here?
stepSynth :: Seconds -> State VoicedSynth Pulse
stepSynth dt = do
  output <- stepSynthVoices dt
  cullSynthVoices
  return output

-- #TODO handle midi timing with fractional samples
-- Chunks the timestep into at most 1 second long chunks
--    this helps when voices end early,
--    as they're only culled once per call to runSynthSteps
--    and leaving them in the EnvDone state will waste time calculating zeros
-- #TODO could the EnvDone state be signalled somehow to automate the culling?
runSynth :: Seconds -> State VoicedSynth [Pulse]
runSynth dt | dt < (1.0/sampleRate) = return []
               | dt > 1.0 = do 
                  firstSec <- runSynthSteps (floor sampleRate) (1.0/sampleRate)
                  remainder <- runSynth (dt - 1.0)
                  return $ firstSec ++ remainder
               | otherwise = let n = floor $ dt*sampleRate
                          in runSynthSteps n (1.0/sampleRate)

runSynthSteps :: Int -> Seconds -> State VoicedSynth [Pulse]
runSynthSteps n dt = do
  output <- runSynthVoices n dt
  cullSynthVoices
  return output

restartVoices :: NoteNumber -> VoicedSynth -> VoicedSynth
restartVoices noteNum voices = mapWhere ((==noteNum) . (view note)) 
                                    restartVoice
                                    voices
-- restartVoices note (voices, notes) = unzip $ mapWhere 
--                                               ((==note) . snd) 
--                                               (first restartVoice) 
--                                               $ zip voices notes
releaseVoices :: NoteNumber -> VoicedSynth -> VoicedSynth
releaseVoices noteNum voices = mapWhere ((==noteNum) . (view note)) releaseVoice voices
-- releaseVoices noteNum (voices, notes) = unzip $ mapWhere 
--                                               ((==noteNum) . snd) 
--                                               (first releaseVoice) 
--                                               $ zip voices notes

-- if there are any voices for that note, set the envelope to EnvAttack state
--  otherwise add a new voice for the note
noteOnSynth :: NoteNumber -> State VoicedSynth ()
noteOnSynth noteNum = modify $ \voices -> 
    if any ((==noteNum) . (view note)) voices
      -- revert envelope to state 1
      then restartVoices noteNum voices
      -- add a new voice for that note
      else (voiceFromNote noteNum):voices
-- noteOnSynth note = modify $ \(voices, notes) -> 
--     if any (== note) notes
--       -- revert envelope to state 1
--       then restartVoices note (voices, notes)
--       -- add a new voice for that note
--       else ((voiceFromNote note):voices, note:notes)

-- set the envelope of any voices with the corrseponding note to EnvRelease state
noteOffSynth :: NoteNumber -> State VoicedSynth ()
noteOffSynth noteNum = modify $ releaseVoices noteNum 




applySynthOp :: State VoicedSynth Pulse -> State FullSynth Pulse
applySynthOp op = state $ \(synth, filtState, filt) -> 
  let (pulse, synth') = runState op synth
      (output, filtState') = runState (filt pulse) filtState
  in (output, (synth', filtState', filt))
mapFilter :: Filter -> [Pulse] -> State FilterState [Pulse]
mapFilter filt [] = return []
mapFilter filt (pulse:pulses) = do
  firstFiltered <- filt pulse 
  restFiltered <- mapFilter filt pulses
  return $ firstFiltered:restFiltered
applySynthOps :: State VoicedSynth [Pulse] -> State FullSynth [Pulse]
applySynthOps op = do 
  pulses <- overState _1 op
  filt <- gets (view _3)
  overState _2 (mapFilter filt pulses) 
    -- applySynthOps op = state $ \(synth, filtState, filt) -> 
    --   let (pulses, synth') = runState op synth
    --       (output, filtState') = runState (mapFilter filt pulses) filtState
    --   in (output, (synth', filtState', filt))
applySynthMod :: State VoicedSynth () -> State FullSynth ()
applySynthMod = overState _1
    -- applySynthMod :: State VoicedSynth () -> State FullSynth ()
    -- applySynthMod op = state $ \(synth, filtState, filt) -> 
    --   let (pulses, synth') = runState op synth
    --   in (pulses, (synth', filtState, filt))


stepFullSynth :: Seconds -> State FullSynth Pulse
stepFullSynth dt = applySynthOp (stepSynth dt)

runFullSynthSteps :: Int -> Seconds -> State FullSynth [Pulse]
runFullSynthSteps n dt = applySynthOps $ runSynthSteps n dt
runFullSynth :: Seconds -> State FullSynth [Pulse]
runFullSynth dt = applySynthOps $ runSynth dt
noteOnFullSynth :: NoteNumber -> State FullSynth ()
noteOnFullSynth note = applySynthMod $ noteOnSynth note
noteOffFullSynth :: NoteNumber -> State FullSynth ()
noteOffFullSynth note = applySynthMod $ noteOffSynth note




data ToyMidi = ToyNoteOn NoteNumber Seconds | ToyNoteOff NoteNumber Seconds | ToyNothing Seconds

synthesiseMidiVoiced :: [ToyMidi] -> State VoicedSynth [Pulse]
synthesiseMidiVoiced [] = return []
synthesiseMidiVoiced ((ToyNoteOn note dt):mids) = do 
  output <- runSynth dt
  noteOnSynth note
  remainder <- synthesiseMidiVoiced mids
  return $ output ++ remainder
synthesiseMidiVoiced ((ToyNoteOff note dt):mids) = do
    output <- runSynth dt
    noteOffSynth note
    remainder <- synthesiseMidiVoiced mids
    return $ output ++ remainder
synthesiseMidiVoiced ((ToyNothing dt):mids) = do
    output <- runSynth dt
    remainder <- synthesiseMidiVoiced mids
    return $ output ++ remainder

hashtagNoFilter :: Filter
hashtagNoFilter = return

lowPass :: Hz -> Seconds -> Filter
lowPass freq dt = \pulse -> state $ \prevs -> 
  -- pulse :: Pulse
  -- prevs :: FilterState == [Pulse]
  let rc = 1/(2*pi*freq)
      alpha = dt / (rc + dt)
      next = alpha*pulse +  (1-alpha) * (head prevs)
  in if null prevs
      then (pulse*alpha, [pulse*alpha])
      else (next, [next])

synthesiseMidiFullSynth :: [ToyMidi] -> State FullSynth [Pulse]
synthesiseMidiFullSynth [] = return []
synthesiseMidiFullSynth ((ToyNoteOn note dt):mids) = do 
  output <- runFullSynth dt
  noteOnFullSynth note
  remainder <- synthesiseMidiFullSynth mids
  return $ output ++ remainder
synthesiseMidiFullSynth ((ToyNoteOff note dt):mids) = do
    output <- runFullSynth dt
    noteOffFullSynth note
    remainder <- synthesiseMidiFullSynth mids
    return $ output ++ remainder
synthesiseMidiFullSynth ((ToyNothing dt):mids) = do
    output <- runFullSynth dt
    remainder <- synthesiseMidiFullSynth mids
    return $ output ++ remainder

testSeq1 :: [ToyMidi]
testSeq1 = [ToyNoteOn 69 0, ToyNoteOff 69 5,
            ToyNothing 5]

testSeq2 :: [ToyMidi]
testSeq2 = [ToyNoteOn 69 0, ToyNoteOn 73 0.5, ToyNoteOn 76 0.5, ToyNoteOn 81 0.5, 
            ToyNoteOff 69 2, ToyNoteOff 73 0, ToyNoteOff 76 0, ToyNoteOff 81 0,
            ToyNothing 50]

defaultSynth :: FullSynth
defaultSynth = (([]), [], lowPass 800 (1/sampleRate))


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
output = map ((max (-1.0)) . (min 1.0)) $ evalState (synthesiseMidiFullSynth testSeq2) defaultSynth
-- output = map ((max (-1.0)) . (min 1.0)) $ evalState (synthesiseMidi testSeq2) ([], [])
-- output = silentNightFull thiccIonian
-- output = performSequence sawSynth dorian tonalCenter jump
-- output = performSequence sawSynth ionian tonalCenter silentNight
-- output = jumpTour
-- output = jumpTour2
-- output = silentNightFull locrian
-- output = jumpFull phrygian
-- output = performSequence pureSynth ionian tonalCenter silentNightChords


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