{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           Data.IntMap                (IntMap)
import qualified Data.IntMap         as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.FilePath
--
import           WordNet.API.Query
import           WordNet.Query
import           WordNet.Parser.Sense
import           WordNet.Type

                         


addSense :: IntMap [SenseItem] -> SenseItem -> IntMap [SenseItem]
addSense !m s = IM.insertWith (++) (s^.sense_soffset) [s] m

 
             
main = do
  -- print (toEnum 44  :: LexicoGrapherFile)
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/dict"
  ss <- (catMaybes <$> parseFile parseSense (dir </> "index.sense"))
  mapM_ print (take 100 ss)

{- 
main' = do
  let dir = "/scratch/wavewave/wordnet/WordNet-3.0/dict"
  ss <- (catMaybes <$> parseFile parseSense (dir </> "index.sense"))
  let m = foldl' addSense IM.empty ss 
  print (IM.size m)
  mapM_ print . drop 50000 . take 51000 .  IM.toAscList $
    (fmap (map (\s->(s^.sense_lemma,headWord s))) m)
    -- (fmap (map (\s->(s^.sense_lemma, ))) m)
-}


{- 
main = do
  db <- loadDB "/scratch/wavewave/wordnet/WordNet-3.0/dict"
  mapM_ print $ take 100 $ HM.toList (db^.senseIdxDB)
-}
