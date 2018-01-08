{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SRL.Analyze.Type.Match where

import           Control.Error.Safe            (rightMay)
import           Control.Lens                  ((^.),(^?),makeLenses,makePrisms,_1,_2,to)
import           Data.Aeson                    (FromJSON,ToJSON)
import           Data.Bifunctor                (second)
import           Data.Function                 (on)
import           Data.Hashable                 (Hashable)
import           Data.List                     (maximumBy)
import           Data.Monoid                   (First(..),(<>))
import           Data.Text                     (Text)
import qualified Data.Text                as T
import           GHC.Generics                  (Generic)
--
import           Data.BitreeZipper             (extractZipperById)
import           Data.Range                    (Range)
import           Lexicon.Type                  (ArgPattern,FNFrame,FNFrameElement,GRel
                                               ,SenseID)
-- import           NLP.Syntax.Clause             (currentCPDPPP)
import           NLP.Syntax.Type.Resolve       (Referent)
import           NLP.Syntax.Type.XBar          (CompVP,Phase(..),Coindex(..),TraceType(..),SpecTP(..)
                                               ,SPhase(..)
                                               ,currentCPDPPP
                                               ,compVPToSpecTP
                                               )
import           NLP.Type.PennTreebankII       (Lemma)



data RangePair = RangePair { _rp_full :: Range
                           , _rp_head :: Range
                           }
               deriving (Generic,Show,Eq,Ord)

makeLenses ''RangePair

instance FromJSON RangePair
instance ToJSON RangePair
instance Hashable RangePair

data EmptyCategoryIndex = ECI_PRO Int --  RangePair --  (Maybe RangePair)
                        | ECI_NULL
                        deriving (Generic, Show, Eq, Ord)

makePrisms ''EmptyCategoryIndex

instance FromJSON EmptyCategoryIndex
instance ToJSON EmptyCategoryIndex
instance Hashable EmptyCategoryIndex

mkPROText (ECI_PRO i) = Just ("PRO_" <> T.pack (show i))
mkPROText _           = Nothing


data EntityInfo = EI { _ei_eci :: Maybe EmptyCategoryIndex -- (Range,Range) -- ^ (full,head) -- fullRange :: Range
                     , _ei_rangePair :: RangePair
                     , _ei_prep      :: Maybe Text
                     , _ei_text      :: Text
                     , _ei_isClause  :: Bool
                     , _ei_isTime    :: Bool
                     }
                deriving Show

makeLenses ''EntityInfo


eiRangeID :: EntityInfo -> Range --  Either EmptyCategoryIndex Range
eiRangeID e = e^.ei_rangePair.rp_full -- e^.ei_id.to (second (^.rp_full))


-- type MatchedElement = -- (Maybe (TraceType,Int),CompVP 'PH1)

data FrameMatchResult = FMR { _fmr_lemmas :: [Text]
                            , _fmr_frame :: FNFrame
                            , _fmr_roles :: Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, Referent (CompVP 'PH1))])
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
