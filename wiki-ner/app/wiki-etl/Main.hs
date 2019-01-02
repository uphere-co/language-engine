module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Pipes ((>->),runEffect)
import qualified Pipes.ByteString as PB (fromHandle,take)
import qualified Pipes.GZip as PZ (decompress)
import qualified Pipes.Prelude as P (chain,drain,print)
import qualified Pipes.Text as PT (take)
import Pipes.Text.Encoding (decodeUtf8,decodeIso8859_1)
import System.IO (withFile, IOMode(..))

fileCategoryLinks :: FilePath
fileCategoryLinks = "/workspace/1/wavewave/wikipedia/enwiki-20181220-categorylinks.sql.gz"

fileCategory :: FilePath
fileCategory = "/workspace/1/wavewave/wikipedia/enwiki-20181220-category.sql.gz"

initiator :: Text
initiator = "INSERT INTO `categorylinks` VALUES"

main :: IO ()
main = do
  putStrLn "wiki-etl"

  withFile fileCategoryLinks ReadMode $ \h -> do
    runEffect $
          void (decodeIso8859_1 (PZ.decompress (PB.fromHandle h)))
      >-> PT.take 2500
      >-> P.chain TIO.putStr
      >-> P.drain
