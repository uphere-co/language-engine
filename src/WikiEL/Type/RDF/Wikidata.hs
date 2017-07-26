{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.RDF.Wikidata where

import           Data.Text                             (Text)

data WikidataObject = Alias           Text
                    | NonEnAlias      Text Text -- type alias
                    | TypedValue      Text Text -- type value
                    | TextValue       Text
                    | NameSpaceObject Text Text -- namespace token
                    | URLObject       Text
                    | UnknownObject   Text
                    deriving(Show, Eq)

data TurtleState = Comma 
                 | Semicolon
                 | End
                 deriving (Show, Eq)

data TurtleRelation = RelationSVO WikidataObject WikidataObject WikidataObject
                    | RelationVO  WikidataObject WikidataObject
                    | RelationO   WikidataObject
                    deriving (Show, Eq)

