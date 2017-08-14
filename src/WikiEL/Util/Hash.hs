module WikiEL.Util.Hash where

import           Data.Digest.XXHash                    (XXHash,xxHash')
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Text                             (Text)

type WordHash = XXHash

wordHash :: Text -> WordHash
wordHash = xxHash' . encodeUtf8
