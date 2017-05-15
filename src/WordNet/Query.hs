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
import WordNet.Type
import WordNet.Parser.Common


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
                      }

makeLenses ''WordNetDB                 

createWordNetDB :: ([IndexItem],[IndexItem],[IndexItem],[IndexItem])
                -> ([DataItem],[DataItem],[DataItem],[DataItem])
                -> WordNetDB 
createWordNetDB ilsts dlsts =
  WNDB (createLemmaSynsetMap (ilsts^._1))
       (createLemmaSynsetMap (ilsts^._2))
       (createLemmaSynsetMap (ilsts^._3))
       (createLemmaSynsetMap (ilsts^._4))
       (createLexItemMap False (dlsts^._1))
       (createLexItemMap True  (dlsts^._2))
       (createLexItemMap False (dlsts^._3))
       (createLexItemMap False (dlsts^._4))


createLemmaSynsetMap :: [IndexItem] -> HM.HashMap Text [Int]
createLemmaSynsetMap = HM.fromList . map (\x->(x^.idx_lemma,x^.idx_synset_offset))

createLexItemMap :: Bool -> [DataItem] -> IM.IntMap ([LexItem],Text)
createLexItemMap isVerb
  = IM.fromList . map (\x->(x^.data_syn_offset,(x^.data_word_lex_id,x^.data_gloss)))

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


lookupLI :: WordNetDB -> POS -> Text -> [([LexItem],Text)]
lookupLI w p t = do
   x <- join . maybeToList $ HM.lookup t (indexDB w p)
   y <- maybeToList $ IM.lookup x (dataDB w p)
   return y

lookupM :: WordNetDB -> POS -> Int -> Maybe ([LexItem],Text)
lookupM w p n = IM.lookup n (dataDB w p)

