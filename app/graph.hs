{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Monoid
--
import           WordNet.Parser.Lexicographer
import           WordNet.Query.SynsetDB
import           WordNet.Type.Lexicographer


-- mkidx =  (\x -> (map formatWord . getSSWords) x) 

{-
newtype SenseKey = SenseKey (LexicographerFile,SSWord)
                 deriving (Show)


filekey t = L.lookup t (map swap lexicographerFileTable)

mkSenseKey
-}


main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      sidx2word = map (\(fname,xs) -> (fname,zip [1..] (map getSSWords xs))) (db^.synsetdb_noun)
      word2sidx = map (\(fname,xs) -> (fname,HM.fromList (f xs))) sidx2word
        where f :: [(Int,[SSWord])] -> [(SSWord,Int)]
              f xs = [(w,i) | (i,ws) <-xs, w <- ws ]
      nounmap_s2w = HM.fromList sidx2word
      nounmap_w2s = HM.fromList word2sidx
  case HM.lookup "noun.food" nounmap_s2w of
    Nothing -> error "Nothing"
    Just xs -> mapM_ print (L.take 10 (reverse xs))
  case HM.lookup "noun.food" nounmap_w2s of
    Nothing -> error "Nothing"
    Just xs -> do
      case parseOnly (fmap (\(w,d,k)->SSWord w d k) p_word_lexid_marker) "ruggelach" of
        Left err -> error err
        Right w -> mapM_ print (HM.lookup w xs)
      case parseOnly (fmap (\(w,d,k)->SSWord w d k) p_word_lexid_marker) "soul_food" of
        Left err -> error err
        Right w -> mapM_ print (HM.lookup w xs)
        
{-

      mapM_ print (take 10 (reverse xs))
  -}

