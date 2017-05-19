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

loadDB :: FilePath
       -> IO ( ([IndexItem], [IndexItem], [IndexItem], [IndexItem])
             , ([DataItem ], [DataItem ], [DataItem ], [DataItem ]) )

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
  return (is,ds)

runSingleQuery input db = do
  case decimal (T.pack input) of
    Left str    -> queryLemma db (T.pack input)
    Right (n,_) -> queryConcept db n

queryLemma db input = do
  putStrLn "-- Noun --"
  mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_N input
  --
  putStrLn "-- Verb --"
  mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_V input
  --
  putStrLn "-- Adjective --"
  mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_A input
  --
  putStrLn "-- Adverb --"
  mapM_ (TIO.putStrLn . format) $ lookupLemma db POS_R input


queryConcept db n = do
  putStrLn "-- Noun --"
  mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_N n
  --
  putStrLn "-- Verb --"
  mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_V n
  --
  putStrLn "-- Adjective --"
  mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_A n
  --
  putStrLn "-- Adverb --"
  mapM_ (TIO.putStrLn . format) $ lookupConcept db POS_R n

format :: ([LexItem],Text) -> Text
format (xs,txt) = T.intercalate "," (map formatLI xs) <> " | " <> txt
