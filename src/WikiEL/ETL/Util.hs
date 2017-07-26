{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Util 
  ( readBlocks
  ) where

import           Data.Text                             (Text)
import           System.IO                             (Handle)
import           Control.Monad                         (unless)
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text                     as T

readBlocks :: Handle -> (a -> Text -> IO a) -> a -> IO ()
readBlocks handle fBlock prevState = do
  chunk <- T.IO.hGetChunk handle
  unless (T.null chunk) $ do     
    state <- fBlock prevState chunk
    readBlocks handle fBlock state
