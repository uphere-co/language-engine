{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module WikiEL.Type where

import           Control.Lens              ( makeLenses, makePrisms )
import           Data.Aeson
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Text                 ( Text )
import qualified Data.Text           as T
import           Data.Vector               ( Vector, toList )
import qualified Data.Vector.Unboxed as UV
import           GHC.Generics              ( Generic )
------ other language-engine
import qualified Graph               as G
import qualified Graph.ETL           as G.E
import qualified Graph.Internal.Hash as H
import           Graph.Internal.Hash       ( WordHash )
import           NLP.Type.NamedEntity      ( NamedEntityClass )
------ wiki-ner
import           WikiEL.Type.Wikidata      ( ItemID )




type SortedEdges = (G.Direction, UV.Vector (H.WordHash, H.WordHash))
type NodeNames   = M.Map H.WordHash G.E.BString

data SortedGraph = SortedGraph SortedEdges NodeNames

