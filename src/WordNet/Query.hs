{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Query where

import           Control.Lens
import           Control.Monad              (join)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
--
import           WordNet.Type
import           WordNet.Parser.Common
--
import           NLP.Type.WordNet

parseFile :: (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst

data WordNetDB = WNDB { _indexNounDB :: HM.HashMap Text [Int]
                      , _indexVerbDB :: HM.HashMap Text [Int]
                      , _indexAdjDB  :: HM.HashMap Text [Int]
                      , _indexAdvDB  :: HM.HashMap Text [Int]
                      , _dataNounDB  :: IM.IntMap ([LexItem],Text)
                      , _dataVerbDB  :: IM.IntMap ([LexItem],Text)
                      , _dataAdjDB   :: IM.IntMap ([LexItem],Text)
                      , _dataAdvDB   :: IM.IntMap ([LexItem],Text)
                      , _senseIdxDB  :: HM.HashMap (Text,Int) Int
                      }

makeLenses ''WordNetDB                 

createWordNetDB :: ([IndexItem],[IndexItem],[IndexItem],[IndexItem])
                -> ([DataItem],[DataItem],[DataItem],[DataItem])
                -> [SenseItem]
                -> WordNetDB 
createWordNetDB ilsts dlsts slists =
  WNDB (createLemmaMap (ilsts^._1))
       (createLemmaMap (ilsts^._2))
       (createLemmaMap (ilsts^._3))
       (createLemmaMap (ilsts^._4))
       (createConceptMap False (dlsts^._1))
       (createConceptMap True  (dlsts^._2))
       (createConceptMap False (dlsts^._3))
       (createConceptMap False (dlsts^._4))
       (createSenseMap slists)

createLemmaMap :: [IndexItem] -> HM.HashMap Text [Int]
createLemmaMap = HM.fromList . map (\x->(x^.idx_lemma,x^.idx_synset_offset))

createConceptMap :: Bool -> [DataItem] -> IM.IntMap ([LexItem],Text)
createConceptMap isVerb
  = IM.fromList . map (\x->(x^.data_syn_offset,(x^.data_word_lex_id,x^.data_gloss)))

createSenseMap :: [SenseItem] -> HM.HashMap (Text,Int) Int
createSenseMap = HM.fromList . map (\x->((x^.sense_lemma,x^.sense_lexid),x^.sense_ss))

indexDB :: WordNetDB -> POS -> HM.HashMap Text [Int]
indexDB w POS_N = w^.indexNounDB
indexDB w POS_V = w^.indexVerbDB
indexDB w POS_A = w^.indexAdjDB
indexDB w POS_R = w^.indexAdvDB

dataDB :: WordNetDB -> POS -> IM.IntMap ([LexItem],Text)
dataDB w POS_N = w^.dataNounDB
dataDB w POS_V = w^.dataVerbDB
dataDB w POS_A = w^.dataAdjDB
dataDB w POS_R = w^.dataAdvDB

senseDB w = w^.senseIdxDB

lookupLemma :: WordNetDB -> POS -> Text -> [([LexItem],Text)]
lookupLemma w p t = do
   n <- join . maybeToList $ HM.lookup t (indexDB w p)
   maybeToList (lookupConcept w p n)

lookupConcept :: WordNetDB -> POS -> Int -> Maybe ([LexItem],Text)
lookupConcept w p n = IM.lookup n (dataDB w p)

lookupSense :: WordNetDB -> Text -> Int -> Maybe Int
lookupSense w t i = HM.lookup (t,i) (senseDB w)
