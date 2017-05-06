{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^?),(^.),(^..), only )
import           Control.Monad (join)
import           Data.Maybe (fromJust,listToMaybe)
import           Data.Text (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read (decimal)
import           Text.Taggy.Lens
--
import           HFrameNet.Type

-- safeHead [] = Nothing
-- safeHead (x:_) = Just x

readDecimal :: Text -> Maybe Int
readDecimal = either (\_ -> Nothing) (Just . fst) . decimal 


p_frame :: Element -> Maybe Frame
p_frame x = Frame <$> (readDecimal =<< (x ^. attr "ID"))
                  <*> x ^. attr "name"
                  <*> x ^. attr "cDate"
                  <*> listToMaybe (x ^.. elements . named (only "definition") . element . contents)
                  <*> mapM p_fe (x ^.. elements . named (only "FE"))
--   in Frame id' name cDate def fes


p_fe :: Element -> Maybe FE
p_fe x = FE <$> (readDecimal =<< (x ^. attr "ID"))
            <*> x ^. attr "name"
            <*> x ^. attr "abbrev"
            <*> x ^. attr "cDate"
            <*> x ^. attr "coreType"
            <*> x ^. attr "fgColor"
            <*> x ^. attr "bgColor"
            <*> listToMaybe (x ^.. elements . named (only "definition") . element . contents)       
  -- in FE id' name abbrev cDate coreType fgColor bgColor def

main :: IO ()
main = do
  txt <- TLIO.readFile "/scratch/wavewave/fndata/fndata-1.7/frame/Revenge.xml"
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  
  -- TLIO.putStrLn txt
  {- 
  let frame = head (txt ^.. (html . allNamed (only "frame")))
      frame_id = fromJust (frame ^. attr "ID")
      frame_name = fromJust (frame ^. attr "name")
      frame_cDate = fromJust (frame ^. attr "cDate")
      frame_definition = head (frame ^.. elements . named (only "definition") . element . contents) -- id
 
  print (frame_id,frame_name,frame_cDate,frame_definition)
  -}
  print (p_frame frame)
