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
import           WordNet.Type.POS


loadDB :: FilePath -> IO WordNetDB
loadDB fp = do
  is <- (,,,) <$> (catMaybes <$> parseFile parseIndex (fp </> "index.noun"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.verb"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adj"))
              <*> (catMaybes <$> parseFile parseIndex (fp </> "index.adv"))
  ds <- (,,,) <$> (catMaybes <$> parseFile (parseData False) (fp </> "data.noun"))
              <*> (catMaybes <$> parseFile (parseData True ) (fp </> "data.verb"))
              <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adj"))
              <*> (catMaybes <$> parseFile (parseData False) (fp </> "data.adv"))
  -- ss <- (catMaybes <$> parseFile parseSense (fp </> "index.sense"))
  return (createWordNetDB is ds {- ss -})


runSingleQuery :: String -> POS -> WordNetDB -> IO ()
runSingleQuery input typ db = do
  case decimal (T.pack input) of
    Left _str    -> queryLemma (T.pack input) typ db
    Right (n,_)  -> querySynset n typ db


queryLemma :: Text -> POS -> WordNetDB -> IO ()
queryLemma input typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --"      >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_N input)
    POS_V -> putStrLn "-- Verb --"      >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_V input)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_A input)
    POS_R -> putStrLn "-- Adverb --"    >> (mapM_ (TIO.putStrLn . formatLemmaSynset) $ lookupLemma db POS_R input)


querySynset :: SynsetOffset -> POS -> WordNetDB -> IO ()
querySynset n typ db = do
  case typ of
    POS_N -> putStrLn "-- Noun --"      >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_N n)
    POS_V -> putStrLn "-- Verb --"      >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_V n)
    POS_A -> putStrLn "-- Adjective --" >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_A n)
    POS_R -> putStrLn "-- Adverb --"    >> (mapM_ (TIO.putStrLn . formatSynset) $ lookupSynset db POS_R n)

{- 
getQueryLemma :: Text -> POS -> WordNetDB -> [(SenseNumber,[LexItemText]
getQueryLemma input typ db = do
  case typ of
    POS_N -> fmap formatLemmaSynset $ lookupLemma db POS_N input
    POS_V -> fmap formatLemmaSynset $ lookupLemma db POS_V input
    POS_A -> fmap formatLemmaSynset $ lookupLemma db POS_A input
    POS_R -> fmap formatLemmaSynset $ lookupLemma db POS_R input


getQuerySynset :: SynsetOffset -> POS -> WordNetDB -> Maybe ([LexItem],Text)
getQuerySynset n typ db = do
  case typ of
    POS_N -> lookupSynset db POS_N n
    POS_V -> lookupSynset db POS_V n
    POS_A -> lookupSynset db POS_A n
    POS_R -> lookupSynset db POS_R n
-}

-- getQuerySense t i db = lookupSense db t i

formatLemmaSynset :: (SenseNumber,[LexItem],Text) -> Text
formatLemmaSynset (SenseNumber n,xs,txt) = "sense: " <> T.pack (show n) <> " | " <> formatSynset (xs,txt)

formatSynset :: ([LexItem],Text) -> Text
formatSynset (xs,txt) = "lexicographer id: " <> T.intercalate "," (map formatLI xs) <>
                        " | " <> txt
