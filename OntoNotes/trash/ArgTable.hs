{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module OntoNotes.Type.ArgTable where

import           Control.Lens
import           Control.Monad                (join)
import           Data.Hashable
import           Data.Foldable
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           Data.Traversable
import           GHC.Generics
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Prop




