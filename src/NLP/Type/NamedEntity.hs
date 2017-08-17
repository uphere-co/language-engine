{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Type.NamedEntity where

import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid

data NamedEntityClass = Org | Person | Loc | Time | Date | Money | Percent| MiscNum | Misc | Other
                      deriving(Show, Eq)

data NamedEntity = NamedEntity { _str  :: Text
                               , _type :: NamedEntityClass}
                 deriving(Show, Eq)

data NamedEntityFrag = NamedEntityFrag { _fstr  :: Text
                                       , _ftype :: NamedEntityClass}
                     deriving(Show, Eq)

data OrderedNamedEntity = OrderedNamedEntity { _order  :: Int
                                             , _entity :: NamedEntity }
                         deriving(Show, Eq)

isSameType :: NamedEntityFrag -> NamedEntityFrag -> Bool
isSameType frag1 frag2 = _ftype frag1 == _ftype frag2


classify :: Text -> Maybe NamedEntityClass 
classify "PERSON"       = Just Person
classify "ORGANIZATION" = Just Org
classify "LOCATION"     = Just Loc
classify "MISC"         = Just Misc     --only for 4 class
classify "TIME"         = Just Time     --only for 7 class
classify "DATE"         = Just Date     --only for 7 class
classify "MONEY"        = Just Money    --only for 7 class
classify "PERCENT"      = Just Percent  --only for 7 class
classify "NUMBER"       = Just MiscNum    --Why HCoreNLP gives this? Ignore it for now.
classify "ORDINAL"      = Just MiscNum    --Why HCoreNLP gives this? Ignore it for now.
classify "DURATION"     = Just MiscNum    --Why HCoreNLP gives this? Ignore it for now.
classify "SET"          = Just Other
classify "O"            = Just Other
classify _              = Nothing

parseStr :: Text -> Text -> NamedEntityFrag
parseStr str t =
  case classify t of
    Nothing -> error ("Unknown named entity class: " ++ T.unpack t)
    Just c  -> NamedEntityFrag str c
