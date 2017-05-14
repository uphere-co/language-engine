{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Options.Applicative
import           System.Console.Haskeline
import           System.FilePath            ((</>))
--
import           WordNet


data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "WordNet lookup")

main = do
  opt <- execParser progOption
  indexverb <- parseFile parseIndex (dir opt </> "index.verb")
  dataverb <- parseFile (parseData True) (dir opt </> "data.verb")
  let indexverb' = catMaybes indexverb
      dataverb' = catMaybes dataverb
      db = createWordNetDB indexverb' dataverb'

  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db (T.pack input')
  
