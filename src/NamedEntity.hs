{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedEntity where

import           Data.Maybe                        (mapMaybe,catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid

data NamedEntityClass = Org | Person | Loc | Time | Date | Other -- | Money | Percent | Misc
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

parseStr :: Text -> Text -> NamedEntityFrag
parseStr str t | t== "PERSON"      = NamedEntityFrag str Person
               | t== "ORGANIZATION"= NamedEntityFrag str Org
               | t== "LOCATION"    = NamedEntityFrag str Loc
               | t== "TIME"        = NamedEntityFrag str Time
               | t== "DATE"        = NamedEntityFrag str Date
               | t== "MONEY"       = NamedEntityFrag str Other--Money
               | t== "PERCENT"     = NamedEntityFrag str Other--Percent
               | t== "MISC"        = NamedEntityFrag str Other--Misc
               | t== "O"           = NamedEntityFrag str Other
parseStr _ t  = error ("Unknown named entity class : " ++ T.unpack t)

partitionFrags :: [NamedEntityFrag] -> [[NamedEntityFrag]]
partitionFrags = foldr f []
  where
    f e [] = [[e]]
    f e xss'@(es:ess) | isSameType e (head es) = (e:es): ess
                      | otherwise              = [e] : xss'

mergeToken :: [NamedEntityFrag] -> Maybe NamedEntity
mergeToken xs'@(NamedEntityFrag str tag : es) | tag /= Other = Just (NamedEntity ss tag) where ss = T.unwords (map _fstr xs')
mergeToken _ = Nothing

mergeTokens :: [NamedEntityFrag] -> [NamedEntity]
mergeTokens es = mapMaybe mergeToken (partitionFrags es)
