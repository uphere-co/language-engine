{-# LANGUAGE OverloadedStrings #-}

module WordNet.Format where

import           Control.Lens
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as T.L
import           Data.Text.Format 
--
import           WordNet.Type
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS

-- formatLemmaSynset :: (SenseNumber,[LexItem],Text) -> Text
-- formatLemmaSynset (SenseNumber n,xs,txt) = "sense: " <> T.pack (show n) <> " | " <> formatSynset (xs,txt)


formatSynset :: POS -> ([LexItem],Text) -> Text
formatSynset p (xs,txt) = "lexicographer id: " <> T.intercalate "," (map (formatLI p) xs) <>
                          " | " <> txt

formatLI :: POS -> LexItem -> Text
formatLI p (LI w i) = T.L.toStrict $ format "{}%{}:cat?:{}::" (w,fromEnum (posToSSType p),unLexID i)


formatLemmaSN :: (Text,SenseNumber) -> Text
formatLemmaSN (lma,SenseNumber n) = lma <> "#" <> T.pack (show n)
