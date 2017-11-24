{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NER.Type where

import Control.Lens.TH (makeLenses)
import Data.Csv
import Data.Text       (Text)
import GHC.Generics

data CSVListedCompany = CSVListedCompany
  { _csvTicker       :: Text
  , _csvName         :: Text
  , _csvLastSale     :: Text
  , _csvMarketCap    :: Text
  , _csvIPOYear      :: Text
  , _csvSector       :: Text
  , _csvIndustry     :: Text
  , _csvSummaryQuote :: Text
  } deriving Show

makeLenses ''CSVListedCompany

instance ToRecord CSVListedCompany where
  toRecord (CSVListedCompany t n ls mc iy s i sq) = record (map toField [t,n,ls,mc,iy,s,i,sq])

instance FromRecord CSVListedCompany where
  parseRecord v = CSVListedCompany <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7

data CompanyInfo = CompanyInfo
  { _ticker :: Text
  , _name :: Text
  , _alias :: [Text] -- This contains official name.
  , _sector :: Text
  , _industry :: Text
  } deriving (Generic, Show)

makeLenses ''CompanyInfo
