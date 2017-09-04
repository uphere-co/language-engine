{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module NLP.Syntax.Type
(

--  --   * reexport from NLP.Type.SyntaxProperty
--   Tense(..)
-- , Voice(..)
-- , Aspect(..)

  -- * Predicate Argument Workspace
  PredArgWorkspace(..), pa_CP, pa_candidate_args

  -- * old types
, SBARType(..)
, STag(..)
, type ClauseTree
, type ClauseTreeZipper
) where

import           Control.Lens
import           Data.Text                              (Text)
--
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Type.XBar

                       
-- | workspace for predicate argument
--
data PredArgWorkspace as a = PAWS { _pa_CP :: CP as
                                  , _pa_candidate_args :: [a]
                                  }

makeLenses ''PredArgWorkspace


---------------
--           --
-- Old types --
--           --
---------------


data SBARType = SB_Word (POSTag,Text)
              | SB_WH   N.PhraseTag
              | SB_None
              deriving Show


data STag = S_RT
          | S_SBAR SBARType
          | S_CL N.ClauseTag
          | S_VP [(Int,(POSTag,Text))]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show

type ClauseTree = Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))

type ClauseTreeZipper = BitreeZipper (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
