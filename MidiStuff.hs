module MidiStuff where
-- {-# LANGUAGE DataKinds #-}
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Codec.Midi
import Codec.ByteString.Parser (runParser, getBytes)

import General

type NoteNumber = Int

-- no microtonal stuff anymore :(
hzFromNoteNumber :: NoteNumber -> Hz
hzFromNoteNumber num = 440.0 * 2 ** ((fromIntegral num - 69.0)/12.0) 


getMidi:: FilePath -> IO Midi
getMidi inputFile = do
  result <- fmap (runParser parseMidi) $ B.readFile inputFile
  case result of
    (Left errorString) -> fail errorString
    (Right midi) -> return midi