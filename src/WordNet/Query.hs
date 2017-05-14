{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Query where

import           Control.Lens
import           Control.Monad              (join)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (catMaybes,maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
--
import WordNet.Type
import WordNet.Parser.Common
import WordNet.Parser.Index
import WordNet.Parser.Data

parseFile :: (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst

data WordNetDB = WNDB { _indexDB :: HM.HashMap Text [Int]
                      , _dataDB  :: IM.IntMap [LexItem] }

makeLenses ''WordNetDB                 

createWordNetDB :: [IndexItem] -> [DataItem] -> WordNetDB 
createWordNetDB ilst dlst = WNDB (createLemmaSynsetMap ilst) (createLexItemMap dlst)

createLemmaSynsetMap :: [IndexItem] -> HM.HashMap Text [Int]
createLemmaSynsetMap = HM.fromList . map (\x->(x^.idx_lemma,x^.idx_synset_offset))

createLexItemMap :: [DataItem] -> IM.IntMap [LexItem]
createLexItemMap = IM.fromList . map (\x->(x^.data_syn_offset,x^.data_word_lex_id))

lookupLI :: WordNetDB -> Text -> [LexItem]
lookupLI w t = do
   x <- join . maybeToList $ HM.lookup t (w^.indexDB)
   join . maybeToList $ IM.lookup x (w^.dataDB)


