{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^?),(^.),(^..), only )
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read
import           Text.Taggy.Lens
--
import           HFrameNet.Type

p_frame :: TL.Text -> Frame
p_frame txt = 
  let frame = head (txt ^.. (html . allNamed (only "frame")))
      id'   = case decimal (fromJust (frame ^. attr "ID")) of
                Left e -> error e
                Right i -> fst i
      name  = fromJust (frame ^. attr "name")
      cDate = fromJust (frame ^. attr "cDate")
      def   = head (frame ^.. elements . named (only "definition") . element . contents) -- id
  in Frame id' name cDate def

main :: IO ()
main = do
  txt <- TLIO.readFile "/scratch/wavewave/fndata/fndata-1.7/frame/Revenge.xml"
  -- TLIO.putStrLn txt
  {- 
  let frame = head (txt ^.. (html . allNamed (only "frame")))
      frame_id = fromJust (frame ^. attr "ID")
      frame_name = fromJust (frame ^. attr "name")
      frame_cDate = fromJust (frame ^. attr "cDate")
      frame_definition = head (frame ^.. elements . named (only "definition") . element . contents) -- id
 
  print (frame_id,frame_name,frame_cDate,frame_definition)
  -}
  print (p_frame txt)
