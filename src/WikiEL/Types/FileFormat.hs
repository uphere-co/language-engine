{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.FileFormat where

import           Data.Text                             (Text)

import           WikiEL.Types.Wikidata


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
