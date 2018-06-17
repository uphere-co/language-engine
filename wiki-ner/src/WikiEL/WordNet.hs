{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEL.WordNet
  where

import           Data.Either                           (Either(..))
import           Data.Maybe                            (Maybe(..))
import qualified Data.Map                      as M
import qualified WikiEL.Type.FileFormat        as F

import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Type.WordNet


type SynsetTable = M.Map ItemID [Synset]

buildWordNetSynsetLookup :: [F.WordNetMappingRow] -> SynsetTable
buildWordNetSynsetLookup mapping = M.fromListWith (++) (map (\x -> (F._itemID x,  [F._synset x])) mapping)

lookupWordNet :: SynsetTable -> Either String ItemID -> Either String [Synset]
lookupWordNet wn (Right key) = 
    case M.lookup key wn of
        Just x -> Right x
        Nothing -> Left "No key."
lookupWordNet _ (Left msg) = Left msg