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


data SynsetMap = SynsetMap { _smap_s2w :: HashMap Int [SSWord]
                           , _smap_w2s :: HashMap SSWord Int
                           -- , _smap_pointers :: HashMap SSWord [SSPointer]
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


  -- let ks = map fst lst

mkIndexDB :: [(Text,[Synset])] -> HashMap Text SynsetMap
mkIndexDB lst =
  HM.fromList . flip map lst $ \(fname,xs) ->
    let sidx2word = zip [1..] (map getSSWords xs)
        word2sidx = [(w,i) | (i,ws) <- sidx2word, w <- ws]
    in (fname,SynsetMap (HM.fromList sidx2word) (HM.fromList word2sidx))

{-
      nounmap_s2w = fmap HM.fromList (HM.fromList sidx2word)
      nounmap_w2s = fmap HM.fromList (HM.fromList word2sidx)
  in fmap HM.fromList . flip mapM ks $ \k -> do
       s2w <- HM.lookup k nounmap_s2w
       w2s <- HM.lookup k nounmap_w2s
       return (k,SynsetMap s2w w2s)
-}

mkSynsetDBFull :: SynsetDB -> SynsetDBFull
mkSynsetDBFull db = SynsetDBFull (mkIndexDB (db^.synsetdb_noun))
                                 (mkIndexDB (db^.synsetdb_verb))
                                 (mkIndexDB (db^.synsetdb_adverb))


findSynonym db txt  = do
  smap <- HM.lookup "noun.food" (db^.synsetdbfull_noun)
  w <- mkSSWord txt
  i <- HM.lookup w (smap^.smap_w2s)
  ws <- HM.lookup i (smap^.smap_s2w)
  return (map formatWord ws)


main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      dbfull = mkSynsetDBFull db
  mapM_ print (findSynonym dbfull "ruggelach")
  mapM_ print (findSynonym dbfull "soul_food")
  mapM_ print (findSynonym dbfull "coffee")
