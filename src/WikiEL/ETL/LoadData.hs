{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.LoadData where

import           Data.Text                             (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.Types.FileFormat
import           WikiEL.ETL.Parser


loadPropertyNames :: PropertyNameFile -> IO [PropertyNameRow]
loadPropertyNames (PropertyNameFile path) = do
  file <- T.IO.readFile path
  let
    rows = map propertyName (T.lines file)
  return rows


