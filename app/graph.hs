{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Monoid
import           Data.Text                        (Text)
--
import           WordNet.Parser.Lexicographer
import           WordNet.Query.SynsetDB
import           WordNet.Type.Lexicographer

data SynsetDBFull
  = SynsetDBFull { _synsetdbfull_noun      :: HashMap Text (HashMap Int [SSWord],HashMap SSWord Int)
                 , _synsetdbfull_verb      :: HashMap Text (HashMap Int [SSWord],HashMap SSWord Int)
                 , _synsetdbfull_adverb    :: HashMap Text (HashMap Int [SSWord],HashMap SSWord Int)
                 {- , _synsetdbfull_adjective :: [(Text,[Either Synset SynsetCluster])] -}
                 }


makeLenses ''SynsetDBFull


mkSSWord :: Text -> Maybe SSWord
mkSSWord txt = 
  case parseOnly (fmap (\(w,d,k)->SSWord w d k) p_word_lexid_marker) txt of
    Left err -> Nothing
    Right w  -> Just w


mkIndexDB :: [(Text,[Synset])] -> Maybe (HashMap Text (HashMap Int [SSWord],HashMap SSWord Int))
mkIndexDB lst =
  let ks = map fst lst
      sidx2word = map (\(fname,xs) -> (fname,zip [1..] (map getSSWords xs))) lst
      word2sidx = map (\(fname,xs) -> (fname,f xs)) sidx2word
        where f :: [(Int,[SSWord])] -> [(SSWord,Int)]
              f xs = [(w,i) | (i,ws) <-xs, w <- ws ]
      nounmap_s2w = fmap HM.fromList (HM.fromList sidx2word)
      nounmap_w2s = fmap HM.fromList (HM.fromList word2sidx)
  in fmap HM.fromList . flip mapM ks $ \k -> do
       s2w <- HM.lookup k nounmap_s2w
       w2s <- HM.lookup k nounmap_w2s
       return (k,(s2w,w2s))


mkSynsetDBFull :: SynsetDB -> Maybe SynsetDBFull
mkSynsetDBFull db = SynsetDBFull <$> mkIndexDB (db^.synsetdb_noun)
                                 <*> mkIndexDB (db^.synsetdb_verb)
                                 <*> mkIndexDB (db^.synsetdb_adverb)


findSynonym db txt  = do
  (foodmap_s2w,foodmap_w2s) <- HM.lookup "noun.food" (db^.synsetdbfull_noun)
  w <- mkSSWord txt
  i <- HM.lookup w foodmap_w2s
  ws <- HM.lookup i foodmap_s2w
  return (map formatWord ws)


main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      Just dbfull = mkSynsetDBFull db
  mapM_ print (findSynonym dbfull "ruggelach")
  mapM_ print (findSynonym dbfull "soul_food")
  mapM_ print (findSynonym dbfull "coffee")
