{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WikiEL.NamedEntity where

import           Data.Maybe                        (mapMaybe,catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid

import           NLP.Type.NamedEntity

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
