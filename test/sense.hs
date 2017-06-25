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
import           System.Environment
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

showResult :: (Show a) => Bool -> Result [a] -> IO ()
showResult doesshowresult er = do 
  case er of
    Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
    Partial f -> case (f "") of
                   Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
                   Done i r -> when doesshowresult (mapM_ print r) >> print (length r) >> print (T.take 100 i)
    Done i r -> when doesshowresult (mapM_ print r) >> print (length r) >> print (T.take 100 i)


  
main0 = do
  -- print (toEnum 44  :: LexicoGrapherFile)
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/dict"
  ss <- (catMaybes <$> parseFile parseSense (dir </> "index.sense"))
  mapM_ print . drop 10000 . Prelude.take 11000 $ ss


testdata = [ "{ lamivudine, 3TC, nucleoside_reverse_transcriptase_inhibitor,@ (a nucleoside reverse transcriptase inhibitor that is very effective in combination with zidovudine in treating AIDS and HIV) }\n"
           , "{ one-hitter, 1\"-hitter, baseball_game,@ (a game in which a pitcher allows the opposing team only one hit) }\n"
           , "{ radiocarbon_dating, carbon_dating, carbon-14_dating, dating,@ (a chemical analysis used to determine the age of organic materials based on their content of the radioisotope carbon 14; believed to be reliable up to 40,000 years) }\n"
           ] 

main = do
  args <- getArgs
  let fp = "/scratch/wavewave/wordnet/WordNet-3.1/b/dbfiles" </> args !! 0
  txt <- TIO.readFile fp


  let er = parse (many1 (p_synset SNoun)) txt
  showResult False er

{- 
main = do
  let txt = "carbon-14_dating"
  -- let txt = "carbon-14"

  let er = parse (replicateM 1 p_word_lexid) txt

  showResult True er
-}
