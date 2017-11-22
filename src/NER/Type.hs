

module NER.Type where

import Data.Csv
import Data.Text (Text)

data Company = Company
  { _ticker       :: Text
  , _name         :: Text
  , _lastSale     :: Text
  , _marketCap    :: Text
  , _ipoYear      :: Text
  , _sector       :: Text
  , _industry     :: Text
  , _summaryQuote :: Text
  } deriving Show

instance ToRecord Company where
  toRecord (Company t n ls mc iy s i sq) = record (map toField [t,n,ls,mc,iy,s,i,sq])

instance FromRecord Company where
  parseRecord v = Company <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6 <*> v .! 7
