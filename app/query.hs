{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Monoid                ((<>))
import qualified Data.Text           as T
import           Data.Text.Read             (decimal)
import           Options.Applicative
import           System.Console.Haskeline
--
import           WordNet.Query
import           WordNet.Type.POS


data ProgOption = ProgOption { dir :: FilePath } deriving Show


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")


progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "WordNet lookup")


main :: IO ()
main = do
  opt <- execParser progOption
  db <- loadDB (dir opt)
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    case decimal (T.pack input) of
      Left _str -> do
        queryLemma (T.pack input) POS_N db
        queryLemma (T.pack input) POS_V db 
        queryLemma (T.pack input) POS_A db 
        queryLemma (T.pack input) POS_R db 
      Right (n,_) -> do
        querySynset n POS_N db
        querySynset n POS_V db
        querySynset n POS_A db
        querySynset n POS_R db

