{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.LoadData where

import           Data.Text                             (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser


loadPropertyNames :: PropertyNameFile -> IO [PropertyNameRow]
loadPropertyNames (PropertyNameFile path) = do
  file <- T.IO.readFile path
  let
    rows = map propertyName (T.lines file)
  return rows


loadEntityReprs :: EntityReprFile -> IO [EntityReprRow]
loadEntityReprs (EntityReprFile path) = do
  file <- T.IO.readFile path
  let
    rows = map entityRepr (T.lines file)
  return rows

loadItemIDs :: ItemIDFile -> IO [ItemIDRow]
loadItemIDs (ItemIDFile path) = do
  file <- T.IO.readFile path
  let
    rows = map itemID (T.lines file)
  return rows

loadSubclassRelations :: SubclassRelationFile -> IO [SubclassRelationRow]
loadSubclassRelations (SubclassRelationFile path) = do
  file <- T.IO.readFile path
  let
    rows = map subclassRelation (T.lines file)
  return rows
