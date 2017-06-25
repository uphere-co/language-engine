{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text
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
-- import           WordNet.API.Query
import           WordNet.Query
import           WordNet.Parser.Sense
import           WordNet.Parser.Lexicographer
import           WordNet.Type
import           WordNet.Type.Lexicographer

                         


addSense :: IntMap [SenseItem] -> SenseItem -> IntMap [SenseItem]
addSense !m s = IM.insertWith (++) (s^.sense_soffset) [s] m

 
             
main0 = do
  -- print (toEnum 44  :: LexicoGrapherFile)
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/dict"
  ss <- (catMaybes <$> parseFile parseSense (dir </> "index.sense"))
  mapM_ print . drop 10000 . Prelude.take 11000 $ ss



main = do
  let fp = "/scratch/wavewave/wordnet/WordNet-3.1/dict/dbfiles/noun.animal"
  txt <- TIO.readFile fp
  let testtxts = (drop 13 (T.lines txt))
  let er = parse (replicateM 10 (p_skipEmptyLine *> p_synset SNoun)) $ T.unlines $ Prelude.take 100 testtxts
  case er of
    Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
    Partial _ -> print "partial"
    Done i r -> mapM_ print r >> print (T.take 100 i)

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
