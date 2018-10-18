module Graph.Internal.Hash
( module Graph.Internal.Hash
) where

import qualified Data.ByteString               as BS
import           Data.Digest.XXHash.FFI                (xxh32)
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Text                             (Text)
import           Data.Word (Word32)


type WordHash = Word32

seed :: Word32
seed = 1234

hash :: BS.ByteString -> WordHash
hash = flip xxh32 seed

wordHash :: Text -> WordHash
wordHash = hash . encodeUtf8
