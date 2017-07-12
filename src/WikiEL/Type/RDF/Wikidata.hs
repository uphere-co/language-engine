{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.RDF.Wikidata where

import           Data.Text                             (Text)

data WikidataObject = Alias      Text
                    | NonEnAlias Text
                    | TypedText  Text
                    | NameSpaceObject Text Text                    
                    | URLObject  Text
                    | UnknownObject Text
                    deriving(Show, Eq)

data TurtleState = Comma 
                 | Semicolon
                 | End
                 deriving (Show, Eq)

data TurtleRelation = RelationSVO WikidataObject WikidataObject WikidataObject
                    | RelationVO  WikidataObject WikidataObject
                    | RelationO   WikidataObject
                    deriving (Show, Eq)

