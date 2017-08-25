{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module NLP.Syntax.Type
(

  -- * reexport from NLP.Type.SyntaxProperty
  Tense(..)
, Voice(..)
, Aspect(..)

  -- * type synonym
, type BitreeICP
, type BitreeZipperICP
, VerbProperty(..), vp_index, vp_lemma, vp_tense, vp_aspect, vp_voice, vp_auxiliary, vp_negation, vp_words
, VerbP(..), vp_maximal_projection, vp_verbProperty, vp_complements
, TP(..), tp_maximal_projection, tp_DP, tp_VP
, CP(..), cp_maximal_projection, cp_complementizer, cp_TP
, DP(..)
, PredArgWorkspace(..), pa_CP, pa_candidate_args

  -- * old types
, SBARType(..)
, STag(..)
, type ClauseTree
, type ClauseTreeZipper
) where

import           Control.Lens
import           Data.Hashable                          (Hashable)
import           Data.Text                              (Text)
import           GHC.Generics                           (Generic)
--
import           Data.BitreeZipper
import           Lexicon.Type                           (ATNode(..))
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data VerbProperty w = VerbProperty { _vp_index  :: Int
                                   , _vp_lemma  :: Lemma
                                   , _vp_tense  :: Tense
                                   , _vp_aspect :: Aspect
                                   , _vp_voice  :: Voice
                                   , _vp_auxiliary :: Maybe (w,(Int,Lemma))
                                   , _vp_negation :: Maybe (w,(Int,Lemma))
                                   , _vp_words  :: [(w,(Int,Lemma))]
                                   }
                    deriving (Show)

makeLenses ''VerbProperty


-- | Projection of Verb Phrase following X-bar theory.
--   The name VP is defined in NLP.Type.PennTreebankII, so I use VerbP.
--
data VerbP = VerbP { _vp_maximal_projection :: BitreeZipperICP '[Lemma]
                   , _vp_verbProperty       :: VerbProperty (BitreeZipperICP '[Lemma])
                   , _vp_complements        :: [BitreeZipperICP '[Lemma]]
                   }

makeLenses ''VerbP


data DP a = SilentPRO | RExp a


-- | Projection of Tense Phrase following X-bar theory, which roughly
--   corresponds to a sentence.
--
data TP = TP { _tp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _tp_DP                 :: Maybe (ATNode (DP (BitreeZipperICP '[Lemma])))
             , _tp_VP                 :: VerbP
             }

makeLenses ''TP

-- | Projection of Complementizer Phrase following X-bar theory
--
data CP = CP { _cp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_complementizer     :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_TP                 :: TP
             }

makeLenses ''CP


-- | workspace for predicate argument
--
data PredArgWorkspace a = PAWS { _pa_CP :: CP
                               , _pa_candidate_args :: [a]
                               }


-- deriving instance (Show a) => Show (PredArgWorkspace a)

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
