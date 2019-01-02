module Main where

import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text.IO as TIO
import Pipes ((>->),runEffect)
import qualified Pipes.ByteString as PB (fromHandle)
import qualified Pipes.GZip as PZ (decompress)
import qualified Pipes.Prelude as P (chain,drain,print)
import qualified Pipes.Text as PT (take)
import Pipes.Text.Encoding (decodeUtf8)
import System.IO (withFile, IOMode(..))

fileCategoryLinks :: FilePath
fileCategoryLinks = "/workspace/1/wavewave/wikipedia/enwiki-20181220-categorylinks.sql.gz"

fileCategory :: FilePath
fileCategory = "/workspace/1/wavewave/wikipedia/enwiki-20181220-category.sql.gz"

-- initiator = "INSERT INTO `categorylinks` VALUES"

main :: IO ()
main = do
  putStrLn "wiki-etl"

  withFile fileCategory ReadMode $ \h -> do
    runEffect $     void (decodeUtf8 (PZ.decompress (PB.fromHandle h)))
                >-> PT.take 20000
                >-> P.chain TIO.putStr
                >-> P.drain
