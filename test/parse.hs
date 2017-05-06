{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^?),(^.),(^..), only )
import           Control.Monad (join)
import           Control.Monad.Identity
import           Data.Maybe (fromJust,listToMaybe)
import           Data.Text (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read (decimal)
import           Data.Time.Clock
import           Data.Time.Format (parseTimeM,defaultTimeLocale)
import           System.FilePath
import           System.Directory
import           Text.Taggy.Lens
--
import           HFrameNet.Parse

process fp = do
  putStrLn fp
  txt <- TLIO.readFile fp
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  case p_frame frame of
    Nothing -> error fp
    Just _ -> return ()
  -- print (p_frame frame)

main :: IO ()
main = do
  let dir = "/scratch/wavewave/fndata/fndata-1.7/frame"
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") cnts
  mapM_ process $ map (\x -> dir </> x) lst
  {- 
  txt <- TLIO.readFile "Revenge.xml"
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  print (p_frame frame)
-}
  
