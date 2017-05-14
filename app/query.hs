{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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

main :: IO ()
main = do
  opt <- execParser progOption
  is@(inoun,iverb,iadj,iadv) <-
    (,,,) <$> (catMaybes <$> parseFile parseIndex (dir opt </> "index.noun"))
          <*> (catMaybes <$> parseFile parseIndex (dir opt </> "index.verb"))
          <*> (catMaybes <$> parseFile parseIndex (dir opt </> "index.adj"))
          <*> (catMaybes <$> parseFile parseIndex (dir opt </> "index.adv"))
  ds@(dnoun,dverb,dadj,dadv) <-
    (,,,) <$> (catMaybes <$> parseFile (parseData False) (dir opt </> "data.noun"))
          <*> (catMaybes <$> parseFile (parseData True ) (dir opt </> "data.verb"))
          <*> (catMaybes <$> parseFile (parseData False) (dir opt </> "data.adj"))
          <*> (catMaybes <$> parseFile (parseData False) (dir opt </> "data.adv"))
  print (length inoun,length iverb,length iadj,length iadv)
  print (length dnoun,length dverb,length dadj,length dadv)

    
  let db = createWordNetDB is ds

  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    putStrLn "-- Noun --"
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db POS_N (T.pack input')
    --
    putStrLn "-- Verb --"
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db POS_V (T.pack input')
    --
    putStrLn "-- Adjective --"
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db POS_A (T.pack input')
    --
    putStrLn "-- Adverb --"
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db POS_R (T.pack input')
