{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.API.Query where

import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Text.Read             (decimal)
import           System.FilePath            ((</>))
--
import           WordNet

loadDB :: FilePath -> IO WordNetDB
loadDB fp = do
  is@(inoun,iverb,iadj,iadv) <-
    (,,,) <$> (catMaybes <$> parseFile parseIndex (fp </> "index.noun"))
          <*> (catMaybes <$> parseFile parseIndex (fp </> "index.verb"))
          <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adj"))
          <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adv"))
  ds@(dnoun,dverb,dadj,dadv) <-
    (,,,) <$> (catMaybes <$> parseFile (parseData False) (fp </> "data.noun"))
          <*> (catMaybes <$> parseFile (parseData True ) (fp </> "data.verb"))
          <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adj"))
          <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adv"))
  return (createWordNetDB is ds)

runSingleQuery input typ db = do
  case decimal (T.pack input) of
    Left str    -> queryLemma (T.pack input) typ db
    Right (n,_) -> queryConcept n typ db

queryLemma input typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --" >> (mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_N input)
    POS_V -> putStrLn "-- Verb --" >> (mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_V input)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_A input)
    POS_R -> putStrLn "-- Adverb --" >> (mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_R input)


queryConcept n typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --" >> (mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_N n)
    POS_V -> putStrLn "-- Verb --" >> (mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_V n)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_A n)
    POS_R -> putStrLn "-- Adverb --" >> (mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_R n)

format :: ([LexItem],Text) -> Text
format (xs,txt) = T.intercalate "," (map formatLI xs) <> " | " <> txt
