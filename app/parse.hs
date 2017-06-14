{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens              ((^?),(^.),(^..), only )
import           Control.Monad             (join)
-- import           Control.Monad.Identity
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

processFrame fp = do
  putStrLn fp
  txt <- TLIO.readFile fp
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  case p_frame frame of
    Nothing -> error fp
    Just _ -> return ()


processLU fp = do
  putStrLn fp
  
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  --print lu
  --print (LU.p_lexUnit lu)
  case LU.p_lexUnit lu of
    Nothing -> error fp
    Just x -> return () --print x
  

data ProgOption = ProgOption { progFrame :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "frame" <> short 'f' <> help "process frame, default is lexical unit")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "parse test for FrameNet")



main :: IO ()
main = do
  opt <- execParser progOption  
  if progFrame opt
     then do
       let dir = "/scratch/wavewave/fndata/fndata-1.7/frame"
       cnts <- getDirectoryContents dir
       let lst = filter (\x -> takeExtensions x == ".xml") cnts
       mapM_ processFrame $ map (\x -> dir </> x) lst
     else do
       let dir = "/scratch/wavewave/fndata/fndata-1.7/lu"
       cnts <- getDirectoryContents dir
       let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
       mapM_ processLU $ map (\x -> dir </> x) lst

