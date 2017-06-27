{-# LANGUAGE OverloadedStrings #-}

module WordNet.Query.SynsetDB where

import           Control.Monad
import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.FilePath
--
import           WordNet.Parser.Lexicographer
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS


nounFiles :: [FilePath]
nounFiles = [ "noun.act"
            , "noun.animal"
            , "noun.artifact"
            , "noun.attribute"
            , "noun.body"
            , "noun.cognition"
            , "noun.communication"
            , "noun.event"
            , "noun.feeling"
            , "noun.food"
            , "noun.group"
            , "noun.location"
            , "noun.motive"
            , "noun.object"
            , "noun.person"
            , "noun.phenomenon"
            , "noun.plant"
            , "noun.possession"
            , "noun.process"
            , "noun.quantity"
            , "noun.relation"
            , "noun.shape"
            , "noun.state"
            , "noun.substance"
            , "noun.time"
            , "noun.Tops"
            ]

verbFiles :: [FilePath]
verbFiles = [ "verb.body"
            , "verb.change"
            , "verb.cognition"
            , "verb.communication"
            , "verb.competition"
            , "verb.consumption"
            , "verb.contact"
            , "verb.creation"
            , "verb.emotion"
            , "verb.motion"
            , "verb.perception"
            , "verb.possession"
            , "verb.social"
            , "verb.stative"
            , "verb.weather"
            ]

adverbFiles :: [FilePath]
adverbFiles = [ "adv.all" ]

adjectiveFiles1 :: [FilePath]
adjectiveFiles1 = [ "adj.pert"
                  , "adj.ppl"
                  ]

-- and adj.all


showResult :: (Show a) => Bool -> Result [a] -> IO ()
showResult doesshowresult er = do 
  case er of
    Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
    Partial f -> case (f "") of
                   Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
                   Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)
                   _ -> error "impossible"
    Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)


process :: FilePath -> SSType -> [FilePath] -> IO [Either String [Maybe Synset]]
process dir typ files = do
  flip mapM files $ \f -> do
    let fp = dir </> f
    putStrLn fp
    txt <- TIO.readFile fp
    let er = parseOnly (many1 (p_synset typ)) txt
    return er
    -- showResult False er
    

processAdjAll :: FilePath -> IO (Either String [Maybe (Either Synset SynsetCluster)])
processAdjAll dir = do
  let fp = dir </> "adj.all"
  putStrLn fp
  txt <- TIO.readFile fp
  let er = parseOnly (many1 p_synset_adj_cluster) txt
  return er
  -- showResult True er

{- 
processAll :: FilePath -> IO ()
processAll dir = do
  er <- processAdjAll dir
  case er of
    Left err -> print err
    Right xs -> print (length xs)
-}

{- 
  process dir Adjective adjectiveFiles1
  process dir Adverb    adverbFiles
  process dir Verb      verbFiles
  process dir Noun      nounFiles
-}
