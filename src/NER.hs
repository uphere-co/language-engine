{-# LANGUAGE OverloadedStrings   #-}

module NER where

import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy.Char8 as BL8
--
import           NER.Load                      (getCompanyList,loadNameTable)




saveCompanyInfo :: IO ()
saveCompanyInfo = do
  nt <- loadNameTable
  clist <- getCompanyList nt
  let bstr = A.encodePretty clist
  BL8.writeFile "company.json" bstr

