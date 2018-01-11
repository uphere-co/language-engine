{-# LANGUAGE OverloadedStrings #-}

module WordNet.Format where

import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as T.L
import           Data.Text.Format 
--
import           WordNet.Type
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS


formatSynset :: (POS,Int) -> ([LexItem],Text) -> Text
formatSynset (p,lfid) (xs,txt) = "lexicographer id: " <> T.intercalate "," (map (formatLI (p,lfid)) xs) <>
                                 " | " <> txt

formatLI :: (POS,Int) -> LexItem -> Text
formatLI (p,lfid) (LI w i) = T.L.toStrict $ format "{}%{}:{}:{}::" (w,fromEnum (posToSSType p),lfid,unLexID i)


formatLemmaSN :: (Text,SenseNumber) -> Text
formatLemmaSN (lma,SenseNumber n) = lma <> "#" <> T.pack (show n)
