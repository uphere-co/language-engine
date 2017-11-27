{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Analyze.Type.Match where

import           Control.Lens                  ((^.),makeLenses,_2)
import           Data.Function                 (on)
import           Data.List                     (maximumBy)
import           Data.Text                     (Text)
import           Data.Range                    (Range)
import           Lexicon.Type                  (ArgPattern,FNFrame,FNFrameElement,GRel
                                               ,SenseID)
import           NLP.Syntax.Type.XBar          (Zipper,X'Tree,TaggedLemma,CompVP)
import           NLP.Type.PennTreebankII       (Lemma,PennTree)


data EntityInfo = EI { _ei_fullRange :: Range
                     , _ei_headRange :: Range
                     , _ei_prep      :: Maybe Text
                     , _ei_text      :: Text
                     }
                deriving Show

makeLenses ''EntityInfo

data FrameMatchResult = FMR { _fmr_frame :: FNFrame
                            , _fmr_roles :: Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])])
                            , _fmr_subframes :: [(FNFrame,Text,[(FNFrameElement,(Bool,Range))])]
                            }

makeLenses ''FrameMatchResult


data DPInfo = DI { _adi_appos :: Maybe EntityInfo
                 , _adi_coref :: Maybe (Range,Range)
                 , _adi_compof :: Maybe EntityInfo
                 , _adi_poss :: [EntityInfo]
                 , _adi_adjs :: [EntityInfo]
                 }
            deriving Show

makeLenses ''DPInfo


data ExceptionalFrame = FrameCopula  | FrameIdiom | FrameLightVerb | FrameNone
                      deriving (Show,Eq,Ord)


data TextifiedFrame = TF { _tf_frameID :: Text
                         , _tf_feCore :: [Text]
                         , _tf_fePeri :: [Text]
                         }
                    deriving (Show,Eq,Ord)

makeLenses ''TextifiedFrame


data ONSenseFrameNetInstance = ONFNInstance { _onfn_senseID :: SenseID
                                            , _onfn_definition :: Text
                                            , _onfn_frame :: Either ExceptionalFrame TextifiedFrame
                                            }
                             deriving (Show,Eq,Ord)


makeLenses ''ONSenseFrameNetInstance



chooseMostFreqFrame :: [(ONSenseFrameNetInstance,Int)] -> [(ONSenseFrameNetInstance,Int)]
chooseMostFreqFrame [] = []
chooseMostFreqFrame xs = [maximumBy (compare `on` (^._2)) xs]


