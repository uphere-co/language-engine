{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Text.Read             (decimal)
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

format :: ([LexItem],Text) -> Text
format (xs,txt) = T.intercalate "," (map formatLI xs) <> " | " <> txt

lookupLemma db input = do
    putStrLn "-- Noun --"
    mapM_ (TIO.putStrLn . format) $ lookupLI db POS_N input
    --
    putStrLn "-- Verb --"
    mapM_ (TIO.putStrLn . format) $ lookupLI db POS_V input
    --
    putStrLn "-- Adjective --"
    mapM_ (TIO.putStrLn . format) $ lookupLI db POS_A input
    --
    putStrLn "-- Adverb --"
    mapM_ (TIO.putStrLn . format) $ lookupLI db POS_R input


lookupMeaning db n = do
    putStrLn "-- Noun --"
    mapM_ (TIO.putStrLn . format) $ lookupM db POS_N n
    --
    putStrLn "-- Verb --"
    mapM_ (TIO.putStrLn . format) $ lookupM db POS_V n
    --
    putStrLn "-- Adjective --"
    mapM_ (TIO.putStrLn . format) $ lookupM db POS_A n
    --
    putStrLn "-- Adverb --"
    mapM_ (TIO.putStrLn . format) $ lookupM db POS_R n





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

    
  let db = createWordNetDB is ds

  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    case decimal (T.pack input) of
      Left str -> lookupLemma db (T.pack input)
      Right (n,_) -> lookupMeaning db n
