{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Util 
  ( readBlocks
  ) where

import           Data.Text                             (Text)
import           System.IO                             (Handle)
import qualified Data.Text.IO                  as T.IO


readBlocksImpl :: Handle -> (Text -> IO ()) -> Text -> IO ()
readBlocksImpl _ _ "" = return ()
readBlocksImpl handle fBlock prevBlock = do
  block <- T.IO.hGetChunk handle
  fBlock block
  readBlocksImpl handle fBlock block

readBlocks :: Handle -> (Text -> IO ()) -> IO ()
readBlocks stdin f = readBlocksImpl stdin f dummy
  where dummy = "asdf"
