{-# LANGUAGE OverloadedStrings   #-}

module NER where

import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BL8
--
import           NER.Load                      (constructCompanyListFromCSV,loadNameTable)




saveCompanyInfo :: FilePath -> FilePath -> IO ()
saveCompanyInfo dataDir fp = do
  nt <- loadNameTable dataDir
  clist <- constructCompanyListFromCSV nt
  let bstr = A.encodePretty clist
  BL8.writeFile fp  bstr   -- "company.json"

