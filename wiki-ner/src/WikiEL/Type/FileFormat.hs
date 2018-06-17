{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.FileFormat where

import           Data.Text                             (Text)

import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet


-- 
newtype PropertyNameFile = PropertyNameFile { unPropertyNameFile :: FilePath }
                         deriving (Show)

data PropertyNameRow = PropertyNameRow { _prop     :: PropertyID                       
                                       , _propName :: Text
                                       }
                     deriving (Show)

--
newtype EntityReprFile = EntityReprFile { unEntityReprFile :: FilePath }
                        deriving (Show)

data EntityReprRow = EntityReprRow { _uid  :: ItemID
                                   , _repr :: ItemRepr
                                   }
                    deriving (Show)

--
newtype ItemIDFile = ItemIDFile { unItemIDFile :: FilePath }
                   deriving (Show)

type ItemIDRow = ItemID

--
newtype SubclassRelationFile = SubclassRelationFile { unSubclassRelationFile :: FilePath }
                             deriving (Show)

data SubclassRelationRow = SubclassRelationRow { _sub :: ItemID 
                                               , _super :: ItemID 
                                               }
                          deriving (Show,Eq)


newtype WordNetMappingFile = WordNetMappingFile { unWordNetMappingFile :: FilePath }
                           deriving (Show)

data WordNetMappingRow = WordNetMappingRow { _pageTitle :: Text
                                           , _pageID    :: PageID
                                           , _itemID    :: ItemID
                                           , _synset    :: Synset
                                           }
                       deriving (Show)

newtype WikiTitleMappingFile = WikiTitleMappingFile { unWikiTitleMappingFile :: FilePath}
                             deriving (Show)
type WikiTitleMappingRow = (ItemID, Text)
