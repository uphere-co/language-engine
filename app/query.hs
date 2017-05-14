{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad              (join)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.Maybe                 (catMaybes,maybeToList)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
--
import WordNet

parseFile :: (Show a) => (Text -> Maybe a) -> FilePath -> IO [Maybe a]
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = filter (not.isComment) (T.lines txt)
  return $ map p lst


format1 x = (x^.data_syn_offset,map formatLI (x^.data_word_lex_id))

format2 x = (x^. idx_lemma, x^.idx_synset_offset)


createLemmaSynsetMap :: [IndexItem] -> HM.HashMap Text [Int]
createLemmaSynsetMap lst =
  let lst' = map (\x->(x^.idx_lemma,x^.idx_synset_offset)) lst
  in HM.fromList lst'

createLexItemMap :: [DataItem] -> IM.IntMap [LexItem]
createLexItemMap lst =
  let lst' = map (\x->(x^.data_syn_offset,x^.data_word_lex_id)) lst
  in IM.fromList lst'

main = do
  indexverb <- parseFile parseIndex "/scratch/wavewave/wordnet/WordNet-3.0/dict/index.verb"
  dataverb <- parseFile (parseData True) "/scratch/wavewave/wordnet/WordNet-3.0/dict/data.verb"
  let indexverb' = catMaybes indexverb
  let dataverb' = catMaybes dataverb
  print (length indexverb,length indexverb')
  print (length dataverb,length dataverb')

  mapM_ (print . format2) $ take 10 indexverb'
  mapM_ (print . format1) $ take 10 dataverb'

  let m1 = createLemmaSynsetMap indexverb'
      m2 = createLexItemMap dataverb'
  print $ HM.lookup "test" m1
  print $ IM.lookup 3133 m2

  print $ do
    x <- join . maybeToList $ HM.lookup "test" m1
    y <- join . maybeToList $ IM.lookup x m2
    return (formatLI y)
  -- print $ filter (\i -> let (_,y,z,w,_) = format2 i in y /= z || y /= w ) $ indexverb'
  
  {- case r of
    Nothing -> return ()
    Just xs -> print (length xs) -}

  
