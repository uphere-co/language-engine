{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Util 
  ( readBlocks
  , readBlocks2
  , readlines
  ) where

import           Data.Text                             (Text)
import           System.IO                             (Handle)
import           Control.Monad                         (unless)
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text                     as T

readBlocks :: Handle -> (a -> Text -> IO a) -> a -> IO ()
readBlocks handle fBlock state = do
  chunk <- T.IO.hGetChunk handle
  unless (T.null chunk) $ do     
    next <- fBlock state chunk
    readBlocks handle fBlock next

readBlocks2 :: Handle -> (a -> Text -> IO a) -> a -> IO a
readBlocks2 handle fBlock state = do
  chunk <- T.IO.hGetChunk handle
  if T.null chunk then
    return state
  else do
    next <- fBlock state chunk
    readBlocks2 handle fBlock next

readlines :: FilePath -> IO [Text]
readlines filepath = do
  content <- T.IO.readFile filepath
  return (T.lines content)

