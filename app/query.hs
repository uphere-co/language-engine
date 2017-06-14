{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens              ((^?),(^.),(^..), only )
import           Control.Monad             (join)
-- import           Control.Monad.Identity
import qualified Data.HashMap.Strict as HM
import           Data.List                 (sort)
import           Data.Maybe                (fromJust,listToMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read            (decimal)
import           Data.Time.Clock
import           Data.Time.Format          (parseTimeM,defaultTimeLocale)
import           Options.Applicative
import           System.FilePath
import           System.Directory
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Frame         (p_frame)
import qualified HFrameNet.Parse.LexUnit as LU (p_lexUnit)
import           HFrameNet.Query

processLU fp = do
  putStrLn fp
  
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  --print lu
  --print (LU.p_lexUnit lu)
  case LU.p_lexUnit lu of
    Nothing -> error fp
    Just x -> return () --print x
  

data ProgOption = ProgOption { progCommand :: String
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strArgument (help "command frame/lu")

  -- switch (long "frame" <> short 'f' <> help "process frame, default is lexical unit")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "parse test for FrameNet")



main :: IO ()
main = do
  opt <- execParser progOption  
  case progCommand opt of
    "frame" -> do
      let dir = "/scratch/wavewave/fndata/fndata-1.7/frame"
      cnts <- getDirectoryContents dir
      let lst = (map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml")) cnts
      framemap <- constructFrameDB lst
      print (HM.lookup "Abandonment" (framemap^.frameDB))
      -- mapM_ processFrame $ map (\x -> dir </> x) lst
    "lu" -> do
      let dir = "/scratch/wavewave/fndata/fndata-1.7/lu"
      cnts <- getDirectoryContents dir
      let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
      mapM_ processLU $ map (\x -> dir </> x) lst
    o -> putStrLn ("cannot understand command " ++ o )
