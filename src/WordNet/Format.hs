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


formatLemmaSynset :: (SenseNumber,[LexItem],Text) -> Text
formatLemmaSynset (SenseNumber n,xs,txt) = "sense: " <> T.pack (show n) <> " | " <> formatSynset (xs,txt)


formatSynset :: ([LexItem],Text) -> Text
formatSynset (xs,txt) = "lexicographer id: " <> T.intercalate "," (map formatLI xs) <>
                        " | " <> txt

formatLI :: LexItem -> Text
formatLI (LI w i) = T.L.toStrict $ format "{}%{}" (w,unLexID i)
