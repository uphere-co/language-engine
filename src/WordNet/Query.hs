{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Query where

import           Control.Lens
import           Control.Monad              (join)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
--
import           WordNet.Parser.Common
import           WordNet.Type
import           WordNet.Type.POS


data WordNetDB = WNDB { _indexNounDB :: HM.HashMap Text [(SenseNumber,SynsetOffset)]
                      , _indexVerbDB :: HM.HashMap Text [(SenseNumber,SynsetOffset)]
                      , _indexAdjDB  :: HM.HashMap Text [(SenseNumber,SynsetOffset)]
                      , _indexAdvDB  :: HM.HashMap Text [(SenseNumber,SynsetOffset)]
                      , _dataNounDB  :: IM.IntMap ([LexItem],Text)
                      , _dataVerbDB  :: IM.IntMap ([LexItem],Text)
                      , _dataAdjDB   :: IM.IntMap ([LexItem],Text)
                      , _dataAdvDB   :: IM.IntMap ([LexItem],Text)
                      -- , _senseIdxDB  :: HM.HashMap (Text,Int) Int
                      }

makeLenses ''WordNetDB                 


createWordNetDB :: ([IndexItem],[IndexItem],[IndexItem],[IndexItem])
                -> ([DataItem],[DataItem],[DataItem],[DataItem])
                -- -> [SenseItem]
                -> WordNetDB 
createWordNetDB ilsts dlsts {- slists -} =
  WNDB (createLemmaMap (ilsts^._1))
       (createLemmaMap (ilsts^._2))
       (createLemmaMap (ilsts^._3))
       (createLemmaMap (ilsts^._4))
       (createSynsetMap (dlsts^._1))
       (createSynsetMap (dlsts^._2))
       (createSynsetMap (dlsts^._3))
       (createSynsetMap (dlsts^._4))
       -- (createSenseMap slists)


createLemmaMap :: [IndexItem] -> HM.HashMap Text [(SenseNumber,SynsetOffset)]
createLemmaMap = HM.fromList . map (\x->(x^.idx_lemma,x^.idx_synset_offset))


createSynsetMap :: [DataItem] -> IM.IntMap ([LexItem],Text)
createSynsetMap = IM.fromList . map (\x->(x^.data_syn_offset.to unSynsetOffset,(x^.data_word_lex_id,x^.data_gloss)))

{- 
createSenseMap :: [SenseItem] -> HM.HashMap (Text,Int) Int
createSenseMap _ = HM.fromList []
-- HM.fromList . map (\x->((x^.sense_sense_key.skey_lemma,x^.sense_lexid),x^.sense_ss))
-}

indexDB :: WordNetDB -> POS -> HM.HashMap Text [(SenseNumber,SynsetOffset)]
indexDB w POS_N = w^.indexNounDB
indexDB w POS_V = w^.indexVerbDB
indexDB w POS_A = w^.indexAdjDB
indexDB w POS_R = w^.indexAdvDB


dataDB :: WordNetDB -> POS -> IM.IntMap ([LexItem],Text)
dataDB w POS_N = w^.dataNounDB
dataDB w POS_V = w^.dataVerbDB
dataDB w POS_A = w^.dataAdjDB
dataDB w POS_R = w^.dataAdvDB

{- 
senseDB :: WordNetDB -> HashMap (Text,Int) Int
senseDB w = w^.senseIdxDB
-}

parseFile :: (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst


lookupLemma :: WordNetDB -> POS -> Text -> [(SenseNumber,[LexItem],Text)]
lookupLemma w p t = do
  (snum,soff) <- join . maybeToList $ HM.lookup t (indexDB w p)
  (ls,desc) <- maybeToList (lookupSynset w p soff)
  return (snum,ls,desc)
   
   -- maybeToList (lookupSynset w p n)


lookupSynset :: WordNetDB -> POS -> SynsetOffset -> Maybe ([LexItem],Text)
lookupSynset w p (SynsetOffset n) = IM.lookup n (dataDB w p)


{- 
lookupSense :: WordNetDB -> Text -> Int -> Maybe Int
lookupSense w t i = HM.lookup (t,i) (w^.senseIdxDB)
-}
