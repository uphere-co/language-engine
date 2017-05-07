{-# LANGUAGE TemplateHaskell #-}

module WordNet where

import Control.Lens
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import WordNet.Type

test :: IO ()
test = do
  putStrLn "test"
  txt <- TIO.readFile "/scratch/wavewave/wordnet/WordNet-3.0/dict/index.verb"
  TIO.putStrLn (T.take 10000 txt )
