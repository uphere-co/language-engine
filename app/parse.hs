{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^?),(^.),(^..), only )
import           Control.Monad (join)
-- import           Control.Monad.Identity
import           Data.List (sort)
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
import qualified HFrameNet.Parse.LexUnit as LU

processFrame fp = do
  putStrLn fp
  txt <- TLIO.readFile fp
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  case p_frame frame of
    Nothing -> error fp
    Just _ -> return ()

main' :: IO ()
main' = do
  let dir = "/scratch/wavewave/fndata/fndata-1.7/frame"
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") cnts
  mapM_ processFrame $ map (\x -> dir </> x) lst

processLU fp = do
  putStrLn fp
  
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  --print lu
  --print (LU.p_lexUnit lu)
  case LU.p_lexUnit lu of
    Nothing -> error fp
    Just x -> return () --print x
  


main :: IO ()
main = do
  putStrLn "lu parse"
  let dir = "/scratch/wavewave/fndata/fndata-1.7/lu"
  --    fp = dir </> "lu6646.xml"
  --processLU fp
  
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
  mapM_ processLU $ map (\x -> dir </> x) lst
  
