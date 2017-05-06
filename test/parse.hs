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
--
import           Debug.Trace

-- safeHead [] = Nothing
-- safeHead (x:_) = Just x

readDecimal :: Text -> Maybe Int
readDecimal = either (\_ -> Nothing) (Just . fst) . decimal 


readBoolean :: Text -> Maybe Bool
readBoolean "true" = Just True
readBoolean "false" = Just False
readBoolean _ = Nothing

p_frame :: Element -> Maybe Frame
p_frame x = Frame <$> (readDecimal =<< (x ^. attr "ID"))
                  <*> x ^. attr "name"
                  <*> x ^. attr "cDate"
                  <*> listToMaybe (x ^.. elements . named (only "definition") . element . contents)
                  <*> mapM p_FE (x ^.. elements . named (only "FE"))
                  <*> trace (show (x ^.. elements . named (only "lexUnit"))) 
                        (mapM p_lexUnit (x ^.. elements . named (only "lexUnit")))


p_FE :: Element -> Maybe FE
p_FE x = FE <$> (readDecimal =<< (x ^. attr "ID"))
            <*> x ^. attr "name"
            <*> x ^. attr "abbrev"
            <*> x ^. attr "cDate"
            <*> x ^. attr "coreType"
            <*> x ^. attr "fgColor"
            <*> x ^. attr "bgColor"
            <*> listToMaybe (x ^.. elements . named (only "definition") . element . contents)
            <*> mapM p_semType (x ^.. elements . named (only "semType"))


p_semType :: Element -> Maybe SemType
p_semType x = SemType <$> (readDecimal =<< (x ^. attr "ID"))
                      <*> x ^. attr "name"


p_lexUnit :: Element -> Maybe LexUnit
p_lexUnit x = LexUnit <$> (readDecimal =<< (x ^. attr "ID"))
                      <*> x ^. attr "name"
                      <*> x ^. attr "POS"
                      <*> x ^. attr "status"
                      <*> x ^. attr "cDate"
                      <*> (readDecimal =<< (x ^. attr "lemmaID"))
                      <*> listToMaybe (x ^.. elements . named (only "definition") . element . contents)
                      <*> mapM p_lexeme  (x ^.. elements . named (only "lexeme"))
                      <*> mapM p_semType (x ^.. elements . named (only "semType"))

p_lexeme :: Element -> Maybe Lexeme
p_lexeme x = Lexeme <$> x ^. attr "name"
                    <*> x ^. attr "POS"
                    <*> (readBoolean =<< (x ^. attr "breakBefore"))
                    <*> (readBoolean =<< (x ^. attr "headword"))
                    <*> (readDecimal =<< (x ^. attr "order"))

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
