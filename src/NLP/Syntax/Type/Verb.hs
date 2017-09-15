{-# LANGUAGE TemplateHaskell #-}

module NLP.Syntax.Type.Verb where

import           Control.Lens
import           Data.Text                 (Text)
--
import           NLP.Type.PennTreebankII   (Lemma(..))
import           NLP.Type.SyntaxProperty   (Tense(..),Voice(..),Aspect(..))


type AuxNegWords w = ([(w,(Int,Lemma))],Maybe (w,(Int,Lemma)),[(w,(Int,Lemma))])

data VerbProperty w = VerbProperty { _vp_index  :: Int
                                   , _vp_lemma  :: Lemma
                                   , _vp_tense  :: Tense
                                   , _vp_aspect :: Aspect
                                   , _vp_voice  :: Voice
                                   , _vp_auxiliary :: [(w,(Int,Lemma))]
                                   , _vp_negation :: Maybe (w,(Int,Lemma))
                                   , _vp_words  :: [(w,(Int,Lemma))]
                                   }
                    deriving (Show)

makeLenses ''VerbProperty




simplifyVProp :: VerbProperty w -> VerbProperty Text
simplifyVProp vprop = VerbProperty { _vp_index     = _vp_index vprop
                                   , _vp_lemma     = _vp_lemma vprop
                                   , _vp_tense     = _vp_tense vprop
                                   , _vp_aspect    = _vp_aspect vprop
                                   , _vp_voice     = _vp_voice vprop
                                   , _vp_auxiliary = fmap f (_vp_auxiliary vprop)
                                   , _vp_negation  = fmap f (_vp_negation vprop)
                                   , _vp_words     = fmap f (_vp_words vprop)
                                   }
  where f (_z,(i,lma)) = (unLemma lma,(i,lma))
