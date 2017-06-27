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
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
--
import           WordNet.Parser.Lexicographer
import           WordNet.Query.SynsetDB
import           WordNet.Type.Lexicographer


data SynsetMap = SynsetMap { _smap_s2w :: HashMap Int [SSWord]
                           , _smap_w2s :: HashMap SSWord Int
                           , _smap_w2p :: HashMap SSWord [SSPointer]
                           }

makeLenses ''SynsetMap

data SynsetDBFull
  = SynsetDBFull { _synsetdbfull_noun      :: HashMap Text SynsetMap
                 , _synsetdbfull_verb      :: HashMap Text SynsetMap
                 , _synsetdbfull_adverb    :: HashMap Text SynsetMap
                 {- , _synsetdbfull_adjective :: [(Text,[Either Synset SynsetCluster])] -}
                 }


makeLenses ''SynsetDBFull


mkSSWord :: Text -> Maybe SSWord
mkSSWord txt = 
  case parseOnly (fmap (\(w,d,k)->SSWord w d k) p_word_lexid_marker) txt of
    Left err -> Nothing
    Right w  -> Just w


mkIndexDB :: [(Text,[Synset])] -> HashMap Text SynsetMap
mkIndexDB lst =
  HM.fromList . flip map lst $ \(fname,xs) ->
    let sidx2word = zip [1..] (map getSSWords xs)
        word2sidx = [(w,i) | (i,ws) <- sidx2word, w <- ws]
        word2pointers = [(w,ps) | xs <- map getSSPairs xs, (w,ps) <- xs ]
    in (fname,SynsetMap (HM.fromList sidx2word) (HM.fromList word2sidx) (HM.fromList word2pointers))


mkSynsetDBFull :: SynsetDB -> SynsetDBFull
mkSynsetDBFull db = SynsetDBFull (mkIndexDB (db^.synsetdb_noun))
                                 (mkIndexDB (db^.synsetdb_verb))
                                 (mkIndexDB (db^.synsetdb_adverb))


llookup k = maybeToList . HM.lookup k

findSynonym :: SynsetDBFull -> Text -> [SSWord]
findSynonym db txt  = do
  smap <- llookup "noun.food" (db^.synsetdbfull_noun)
  w <- maybeToList $ mkSSWord txt
  i <- llookup w (smap^.smap_w2s)
  ws <- llookup i (smap^.smap_s2w)
  w' <- ws
  return w'


findHypernym :: SynsetDBFull -> Text -> [SSWord]
findHypernym db txt  = do
  smap <- llookup "noun.food" (db^.synsetdbfull_noun)
  w <- maybeToList $ mkSSWord txt
  ps <- llookup w (smap^.smap_w2p)
  h <- map (^.ssp_word) . filter (\p -> p^.ssp_pointer_symbol == Hypernym) $ ps
  return h

main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      dbfull = mkSynsetDBFull db
  (print . map formatWord) (findSynonym dbfull "ruggelach")
  (print . map formatWord) (findSynonym dbfull "soul_food")
  (print . map formatWord) (findSynonym dbfull "coffee")
  putStrLn "--------------"
  (print . map formatWord) (findHypernym dbfull "coffee")  
