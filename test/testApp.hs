{-# LANGUAGE OverloadedStrings #-}

module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           System.IO                             (Handle,stdin,stdout)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO


readBlocks :: Handle -> (Text -> IO ()) -> Text -> IO ()
readBlocks _ _ "" = return ()
readBlocks handle fBlock prevBlock = do
  block <- T.IO.hGetChunk handle
  fBlock block
  readBlocks handle fBlock block

f :: Text -> IO ()
f block = do
  let
    lines = T.lines block
  mapM_ (T.IO.hPutStrLn stdout) lines  

main1 = readBlocks stdin f "asdf"

main :: IO ()
main = main1
