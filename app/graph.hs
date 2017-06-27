{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet               as HS
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
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

findSynonym :: SynsetDBFull -> (Text,SSWord) -> [(Text,SSWord)]
findSynonym db (lexfile,w) = do
  smap <- llookup lexfile (db^.synsetdbfull_noun)
  -- w <- maybeToList $ mkSSWord txt
  i <- llookup w (smap^.smap_w2s)
  ws <- llookup i (smap^.smap_s2w)
  w' <- ws
  return (lexfile,w')


findHypernym :: SynsetDBFull -> (Text,SSWord) -> [(Text,SSWord)]
findHypernym db (lexfile,w) = do
  smap <- llookup lexfile (db^.synsetdbfull_noun)
  --  w <- maybeToList $ mkSSWord txt
  ps <- llookup w (smap^.smap_w2p)
  let hps = filter (\p -> p^.ssp_pointer_symbol == Hypernym) ps
  h <- map (\x->(fromMaybe lexfile (getLexFile x),x^.ssp_word)) hps
  return h

findHypernymHierarchy :: SynsetDBFull -> (Text,SSWord) -> HashSet ((Text,SSWord),(Text,SSWord))
findHypernymHierarchy db lw =
  let xs = findHypernym db lw
      --  ys = map (findHypernymHierarchy db xs 
  in case xs of
       [] -> HS.empty
       _  -> HS.unions (HS.fromList (map (lw,) xs) : map (findHypernymHierarchy db) xs)


main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      dbfull = mkSynsetDBFull db
  {- (print . map (formatWord.snd)) (findSynonym dbfull ("noun.food",fromJust (mkSSWord "ruggelach")))
  (print . map (formatWord.snd)) (findSynonym dbfull ("noun.food",fromJust (mkSSWord "soul_food")))
  (print . map (formatWord.snd)) (findSynonym dbfull ("noun.food",fromJust (mkSSWord "coffee"))) 
  putStrLn "--------------" -}
  putStrLn "-----------------------------"  
  putStrLn "hypernym hierarchy for coffee"
  putStrLn "-----------------------------"
  mapM_ (TIO.putStrLn . formatGraph) . HS.toList $ 
    findHypernymHierarchy dbfull ("noun.food",fromJust (mkSSWord "coffee"))
  putStrLn "-----------------------------"  
  putStrLn "hypernym hierarchy for lion  "
  putStrLn "-----------------------------"
  mapM_ (TIO.putStrLn . formatGraph) . HS.toList $ 
    findHypernymHierarchy dbfull ("noun.animal",fromJust (mkSSWord "lion"))
  putStrLn "-----------------------------"  
  putStrLn "hypernym hierarchy for doctor"
  putStrLn "-----------------------------"
  mapM_ (TIO.putStrLn . formatGraph) . HS.toList $ 
    findHypernymHierarchy dbfull ("noun.person",fromJust (mkSSWord "doctor"))



formatGraph (x,y) =
  -- T.pack (show x ) <> " -> " <> T.pack (show y)
  formatWord (snd x) <> " -> " <> formatWord (snd y)
