{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Analyze.Type.Match where

import           Control.Lens                  ((^.),(^?),makeLenses,_1,_2)
import           Data.Function                 (on)
import           Data.List                     (maximumBy)
import           Data.Monoid                   (First(..))
import           Data.Text                     (Text)
--
import           Data.BitreeZipper             (extractZipperById)
import           Data.Range                    (Range)
import           Lexicon.Type                  (ArgPattern,FNFrame,FNFrameElement,GRel
                                               ,SenseID)
import           NLP.Syntax.Clause             (currentCPDPPP)
import           NLP.Syntax.Type.XBar          (CompVP)
import           NLP.Type.PennTreebankII       (Lemma)


data EntityInfo = EI { _ei_fullRange :: Range
                     , _ei_headRange :: Range
                     , _ei_prep      :: Maybe Text
                     , _ei_text      :: Text
                     , _ei_isClause  :: Bool
                     , _ei_isTime    :: Bool
                     }
                deriving Show

makeLenses ''EntityInfo

data FrameMatchResult = FMR { _fmr_lemmas :: [Text]
                            , _fmr_frame :: FNFrame
                            , _fmr_roles :: Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP)])
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

emptyDPInfo = DI Nothing Nothing Nothing [] []

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


{-
chooseMostFreqFrame :: [((ONSenseFrameNetInstance,Int),a)] -> [((ONSenseFrameNetInstance,Int),a)]
chooseMostFreqFrame [] = []
chooseMostFreqFrame xs = [maximumBy (compare `on` (^._1._2)) xs]
-}


cpdpppFromX'Tree x'tr rng prm
  = (^? prm) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng)) x'tr)
