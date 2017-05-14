{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (catMaybes,maybeToList)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
import           System.Console.Haskeline
--
import WordNet

parseFile :: (Show a) => (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst


format1 x = (x^.data_syn_offset,map formatLI (x^.data_word_lex_id))

format2 x = (x^. idx_lemma, x^.idx_synset_offset)

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


main = do
  indexverb <- parseFile parseIndex "/scratch/wavewave/wordnet/WordNet-3.0/dict/index.verb"
  dataverb <- parseFile (parseData True) "/scratch/wavewave/wordnet/WordNet-3.0/dict/data.verb"
  let indexverb' = catMaybes indexverb
      dataverb' = catMaybes dataverb
      db = createWordNetDB indexverb' dataverb'

  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    mapM_ (TIO.putStrLn . formatLI) $ lookupLI db (T.pack input')
  {- print (length indexverb,length indexverb')
  print (length dataverb,length dataverb')

  mapM_ (print . format2) $ take 10 indexverb'
  mapM_ (print . format1) $ take 10 dataverb' -}

  {- 
  let m1 = createLemmaSynsetMap indexverb'
      m2 = createLexItemMap dataverb'
  print $ HM.lookup "test" m1
  print $ IM.lookup 3133 m2

  print $ do
    x <- join . maybeToList $ HM.lookup "test" m1
    y <- join . maybeToList $ IM.lookup x m2
    return (formatLI y) -}

  
