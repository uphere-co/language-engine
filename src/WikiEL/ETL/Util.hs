{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Util 
  ( readBlocks
  ) where

import           Data.Text                             (Text)
import           System.IO                             (Handle)
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text                     as T


readBlocksImpl :: Handle -> (Text -> IO ()) -> Text -> IO ()
readBlocksImpl handle fBlock prevPartialBlock = do
  chunk <- T.IO.hGetChunk handle
  case chunk of
      "" -> return ()
      _  -> do
              let (block,partialBlock) = T.breakOnEnd "\n" chunk
              fBlock (T.append prevPartialBlock block)
              readBlocksImpl handle fBlock partialBlock

readBlocks :: Handle -> (Text -> IO ()) -> IO ()
readBlocks stdin f = readBlocksImpl stdin f ""
