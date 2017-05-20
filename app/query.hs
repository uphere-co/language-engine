{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Text.Read             (decimal)
import           Options.Applicative
import           System.Console.Haskeline
import           System.FilePath            ((</>))
--
import           PropBank

main' = do
  putStrLn "parse propbank xml files"
  x <- constructFrameDB "/home/wavewave/repo/srcc/propbank-frames/frames"
  print (HM.size (x^.frameDB))

main = do
  frame <- parseFrameFile "/home/wavewave/repo/srcc/propbank-frames/frames/salivate.xml"
  print frame
