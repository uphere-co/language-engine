{-# LANGUAGE OverloadedStrings   #-}

module NER where

import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BL8
--
import           NER.Load                      (getCompanyListFromJSON,constructCompanyListFromCSV,loadNameTable)




saveCompanyInfo :: FilePath -> IO ()
saveCompanyInfo fp = do
  nt <- loadNameTable
  clist <- constructCompanyListFromCSV nt
  let bstr = A.encodePretty clist
  BL8.writeFile fp  bstr   -- "company.json"

