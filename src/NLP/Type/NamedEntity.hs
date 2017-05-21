{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Type.NamedEntity where

import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid

data NamedEntityClass = Org | Person | Loc | Time | Date | Other
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
               | t== "O"           = NamedEntityFrag str Other
parseStr _ _  = error "Unknown named entity class"

partitionFrags :: [NamedEntityFrag] -> [[NamedEntityFrag]]
partitionFrags frags = foldr f [] frags
  where
    f e [] = [[e]]
    f e xss'@(es:ess) | isSameType e (head es) = (e:es): ess
                      | otherwise              = [e] : xss'

mergeToken :: [NamedEntityFrag] -> Maybe NamedEntity
mergeToken xs'@(NamedEntityFrag str tag : es) | tag /= Other = Just (NamedEntity ss tag) where ss = T.unwords (map _fstr xs')
mergeToken _ = Nothing

mergeTokens :: [NamedEntityFrag] -> [NamedEntity]
mergeTokens es = catMaybes (map mergeToken (partitionFrags es))
