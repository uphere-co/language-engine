{-# LANGUAGE OverloadedStrings #-}

module WordNet.Format where

import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text    as T
--
import           WordNet.Type


formatLemmaSynset :: (SenseNumber,[LexItem],Text) -> Text
formatLemmaSynset (SenseNumber n,xs,txt) = "sense: " <> T.pack (show n) <> " | " <> formatSynset (xs,txt)

formatSynset :: ([LexItem],Text) -> Text
formatSynset (xs,txt) = "lexicographer id: " <> T.intercalate "," (map formatLI xs) <>
                        " | " <> txt
