{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.Monoid
--
import           WordNet.Query.SynsetDB
import           WordNet.Type.Lexicographer


mkidx =  (\x -> (map formatWord . getSSWords) x) 

main :: IO ()
main = do
  let fp = "wordnet31hs.bin"
  lbstr <- BL.readFile fp
  let db = decode lbstr :: SynsetDB
      nounmap = HM.fromList $ map (\(k,xs) -> (k,zip [1..] (map mkidx xs))) (db^.synsetdb_noun)
  case HM.lookup "noun.food" nounmap of
    Nothing -> error "Nothing"
    Just xs -> mapM_ print (take 10 (reverse xs))


