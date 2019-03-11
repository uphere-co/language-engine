{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module WikiEL.WikiEntityTagger 
  ( module WikiEL.WikiEntityTagger
  , WordHash
  ) where

import           Control.Monad.ST                      (runST)
import           Control.Arrow                         (second)
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,unsafeThaw,modify)
import           Data.Vector.Unboxed                   ((!))
import           Data.Vector.Algorithms.Intro          (sortBy)
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
--
import           Graph.Internal.Hash                   (WordHash,wordHash)
import           WikiEL.Type                           (IRange(..),NameUIDTable(..),WordsHash)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Type.FileFormat                (EntityReprFile,EntityReprRow(..))
import           WikiEL.ETL.LoadData                   (loadEntityReprs)
import           WikiEL.BinarySearch                   (binarySearchLRByBounds)
import qualified WikiEL.Type.Wikidata         as Wiki

{-|
  WikiEL.WikiEntityTagger is a text-match based Wikipedia/Wikidata entity tagger. 
  With greedy algorithm, it tags a phrase if it matches with a name or an alias of an entity.
-}







loadWETagger :: EntityReprFile -> IO NameUIDTable
loadWETagger file = do
     reprs <- loadEntityReprs file
     let 
       table = buildEntityTable reprs
     return table 


