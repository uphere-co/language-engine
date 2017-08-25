module WikiEL.Util.Hash
  ( module WikiEL.Util.Hash
  ) where

import           Data.Digest.XXHash                    (XXHash,xxHash')
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Text                             (Text)
import qualified Data.ByteString               as BS


type WordHash = XXHash

hash :: BS.ByteString -> XXHash
hash = xxHash'

wordHash :: Text -> WordHash
wordHash = hash . encodeUtf8
