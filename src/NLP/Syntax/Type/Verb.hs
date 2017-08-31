{-# LANGUAGE TemplateHaskell #-}

module NLP.Syntax.Type.Verb where

import           Control.Lens
--
import           NLP.Type.PennTreebankII                (Lemma(..))
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))


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
