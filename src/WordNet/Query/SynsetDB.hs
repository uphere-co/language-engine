{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Query.SynsetDB where

import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Binary
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           GHC.Generics
import           System.FilePath
--
import           WordNet.Parser.Lexicographer
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS

 
data SynsetDB
  = SynsetDB { _synsetdb_noun      :: [(Text,[Synset])]
             , _synsetdb_verb      :: [(Text,[Synset])]
             , _synsetdb_adverb    :: [(Text,[Synset])]
             , _synsetdb_adjective :: [(Text,[Either Synset SynsetCluster])]
             }
  deriving (Generic)            

instance Binary SynsetDB

makeLenses ''SynsetDB


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


process :: FilePath -> SSType -> [FilePath] -> IO [(Text,[Synset])] -- -> IO [Either String [Maybe Synset]]
process dir typ files = do
  (\func -> foldlM func [] files) $ \xs f -> do
    let fp = dir </> f
    putStrLn fp
    txt <- TIO.readFile fp
    let er = parseOnly (many1 (p_synset typ)) txt
    case er of
      Left err -> error err
      Right lst -> let x = (T.pack f,catMaybes lst) in return (x:xs)
    

processAdjAll :: FilePath -> IO (Text,[Either Synset SynsetCluster])
processAdjAll dir = do
  let fp = dir </> "adj.all"
  putStrLn fp
  txt <- TIO.readFile fp
  let er = parseOnly (many1 p_synset_adj_cluster) txt
  case er of
    Left err -> error err
    Right lst -> return ("adj.all",catMaybes lst)
  -- showResult True er

processAll :: FilePath -> IO SynsetDB
processAll dir = do
  mnoun <- process dir Noun nounFiles
  mverb <- process dir Verb   verbFiles
  madv  <- process dir Adverb adverbFiles
  madj1 <- map (\(f,xs) -> (f,map Left xs)) <$> process dir Adjective adjectiveFiles1
  madj2 <- processAdjAll dir
  return (SynsetDB mnoun mverb madv (madj2:madj1))

{-  
processAll :: FilePath -> IO ()
processAll dir = do
  er <- processAdjAll dir
  case er of
    Left err -> print err
    Right xs -> print (length xs)
 
  process dir Adjective adjectiveFiles1
  process dir Adverb    adverbFiles
  process dir Verb      verbFiles
  process dir Noun      nounFiles
-}
