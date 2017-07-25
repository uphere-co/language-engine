{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Util 
  ( readBlocks
  , readBlocks2
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

readBlocksImpl2 :: Handle -> (a -> Text -> IO a) -> a -> Text -> IO ()
readBlocksImpl2 handle fBlock prevState prevPartialBlock = do
  chunk <- T.IO.hGetChunk handle
  case chunk of
      "" -> return ()
      _  -> do
              let (block,partialBlock) = T.breakOnEnd "\n" chunk
              state <- fBlock prevState (T.append prevPartialBlock block)
              readBlocksImpl2 handle fBlock state partialBlock

readBlocks2 :: Handle -> (a -> Text -> IO a) -> a -> IO ()
readBlocks2 stdin f state = readBlocksImpl2 stdin f state ""
