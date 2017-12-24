{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.Syntax.Type.PreAnalysis where

import           Control.Lens
import           Data.Text               (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.NamedEntity    (NamedEntityClass(..))
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos         (TagPos,TokIdx)
import           WordNet.Type.Lexicographer (LexicographerFile)


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))

data MarkType = MarkTime | MarkEntity NamedEntityClass
              deriving (Show,Eq)

makePrisms ''MarkType

data PreAnalysis t = PreAnalysis { _pennTree  :: BitreeICP t
                                 , _lemmaList :: [(Int,(Lemma,Text))]
                                 , _synsetList :: [(Int,LexicographerFile)]
                                 , _tagList   :: [TagPos TokIdx MarkType]
                                 }

makeLenses ''PreAnalysis

