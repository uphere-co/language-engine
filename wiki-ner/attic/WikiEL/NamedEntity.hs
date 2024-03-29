{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WikiEL.NamedEntity where

import           Data.Maybe                        (mapMaybe)
import qualified Data.Text                  as T
--
import           NLP.Type.NamedEntity

{-|
  CoreNLP NER tags per word. But named entities are often phrases.
  The word fragments should be merged to recover entities.
-}
partitionFrags :: [NamedEntityFrag] -> [[NamedEntityFrag]]
partitionFrags = foldr f []
  where
    f e [] = [[e]]
    f e xss'@(es:ess) | isSameType e (head es) = (e:es): ess
                      | otherwise              = [e] : xss'

mergeToken :: [NamedEntityFrag] -> Maybe NamedEntity
mergeToken xs'@(NamedEntityFrag _str tag : _es) | tag /= Other = Just (NamedEntity ss tag) where ss = T.unwords (map _fstr xs')
mergeToken _ = Nothing

mergeTokens :: [NamedEntityFrag] -> [NamedEntity]
mergeTokens es = mapMaybe mergeToken (partitionFrags es)
