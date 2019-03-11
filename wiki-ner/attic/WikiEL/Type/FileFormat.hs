{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.FileFormat where

import           Data.Text                             (Text)

import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet


-- 
newtype PropertyNameFile = PropertyNameFile { unPropertyNameFile :: FilePath }
                         deriving (Show)


--
newtype SubclassRelationFile = SubclassRelationFile { unSubclassRelationFile :: FilePath }
                             deriving (Show)



newtype WordNetMappingFile = WordNetMappingFile { unWordNetMappingFile :: FilePath }
                           deriving (Show)


newtype WikiTitleMappingFile = WikiTitleMappingFile { unWikiTitleMappingFile :: FilePath}
                             deriving (Show)

