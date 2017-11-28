{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER where

import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                  (find,foldl')
import           Data.Maybe                 (catMaybes,fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import qualified Data.Vector          as V
--
import           NER.Load                      (getCompanyList,loadNameTable)
import           NER.Type
import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Run                    (reprFileG)
import qualified WikiEL.Type.FileFormat  as FF
import qualified WikiEL.Type.Wikidata    as WD


saveCompanyInfo = do
  nt <- loadNameTable
  clist <- getCompanyList nt
  let bstr = A.encodePretty clist
  BL8.writeFile "company.json" bstr

