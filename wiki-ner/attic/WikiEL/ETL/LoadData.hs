{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.LoadData where

import           Data.Text                             (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Map                      as M

import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser

import qualified WikiEL.Type.Equity            as E
import qualified WikiEL.Type.Wikidata          as W




loadPropertyNames :: PropertyNameFile -> IO [PropertyNameRow]
loadPropertyNames = loadFile unPropertyNameFile propertyName




loadItemIDs :: ItemIDFile -> IO [ItemIDRow]
loadItemIDs = loadFile unItemIDFile itemID


loadSubclassRelations :: SubclassRelationFile -> IO [SubclassRelationRow]
loadSubclassRelations = loadFile unSubclassRelationFile subclassRelation


loadWordNetMapping :: WordNetMappingFile -> IO [WordNetMappingRow]
loadWordNetMapping = loadFile unWordNetMappingFile wordNetMapping


loadCompanySymbol :: FilePath -> IO (M.Map W.ItemID E.Symbol)
loadCompanySymbol listedCompanyFile = do
  file <- T.IO.readFile listedCompanyFile
  let 
    xs = T.lines file
    companies = map publicCompany xs
    tickers  = map (\(_,_,_,symbol,_,itemid) -> (itemid, symbol)) companies
    tickerMap = M.fromList tickers
  return tickerMap

