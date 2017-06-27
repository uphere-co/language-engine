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
                                              

{-
                 , _synsetdbfull_verb      :: [(Text,[Synset])]
                 , _synsetdbfull_adverb    :: [(Text,[Synset])]
                 , _synsetdbfull_adjective :: [(Text,[Either Synset SynsetCluster])] -}
                 }


makeLenses ''SynsetDBFull

-- mkidx =  (\x -> (map formatWord . getSSWords) x) 

{-
newtype SenseKey = SenseKey (LexicographerFile,SSWord)
                 deriving (Show)


filekey t = L.lookup t (map swap lexicographerFileTable)

mkSenseKey
-}

mkSSWord :: Text -> Maybe SSWord
mkSSWord txt = 
  case parseOnly (fmap (\(w,d,k)->SSWord w d k) p_word_lexid_marker) txt of
    Left err -> Nothing
    Right w  -> Just w


mkIndexDB :: SynsetDB -> Maybe SynsetDBFull
mkIndexDB db =
  let ks = map fst (db^.synsetdb_noun)
      sidx2word = map (\(fname,xs) -> (fname,zip [1..] (map getSSWords xs))) (db^.synsetdb_noun)
      word2sidx = map (\(fname,xs) -> (fname,f xs)) sidx2word
        where f :: [(Int,[SSWord])] -> [(SSWord,Int)]
              f xs = [(w,i) | (i,ws) <-xs, w <- ws ]
      nounmap_s2w = fmap HM.fromList (HM.fromList sidx2word)
      nounmap_w2s = fmap HM.fromList (HM.fromList word2sidx)
  in fmap (SynsetDBFull . HM.fromList) . flip mapM ks $ \k -> do
       s2w <- HM.lookup k nounmap_s2w
       w2s <- HM.lookup k nounmap_w2s
       return (k,(s2w,w2s))


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
      Just dbfull = mkIndexDB db
  mapM_ print (findSynonym dbfull "ruggelach")
  mapM_ print (findSynonym dbfull "soul_food")
  mapM_ print (findSynonym dbfull "coffee")
{-                   
  case HM.lookup "noun.food" nounmap_w2s of
    Nothing -> error "Nothing"
    Just xs -> do
  -}                     
                      
--      mapM_ print (mkSSWord "soul_food" >>= \w -> HM.lookup w xs)
        
{-

  case HM.lookup "noun.food" nounmap_s2w of
    Nothing -> error "Nothing"
    Just xs -> mapM_ print (L.take 10 (reverse xs))

      mapM_ print (take 10 (reverse xs))
  -}

