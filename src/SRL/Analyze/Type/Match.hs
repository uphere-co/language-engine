{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SRL.Analyze.Type.Match where

import           Control.Lens                  ((^.),makeLenses,makePrisms)
import           Data.Aeson                    (FromJSON,ToJSON)
import           Data.Hashable                 (Hashable)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                as T
import           GHC.Generics                  (Generic)
--
import           Data.Range                    (Range)
import           Lexicon.Type                  (ArgPattern,FNFrame,FNFrameElement,GRel,SenseID)
import           NLP.Syntax.Type.Resolve       (Referent)
import           NLP.Syntax.Type.XBar          (CompVP,Phase(..))


data RangePair = RangePair { _rp_full :: Range
                           , _rp_head :: Range
                           }
               deriving (Generic,Show,Eq,Ord)

makeLenses ''RangePair

instance FromJSON RangePair
instance ToJSON RangePair
instance Hashable RangePair

data EmptyCategoryIndex = ECI_PRO Int
                        | ECI_NULL
                        deriving (Generic, Show, Eq, Ord)

makePrisms ''EmptyCategoryIndex

instance FromJSON EmptyCategoryIndex
instance ToJSON EmptyCategoryIndex
instance Hashable EmptyCategoryIndex


mkPROText :: EmptyCategoryIndex -> Maybe Text
mkPROText (ECI_PRO i) = Just ("PRO_" <> T.pack (show i))
mkPROText _           = Nothing


data EntityInfo = EI { _ei_eci :: Maybe EmptyCategoryIndex
                     , _ei_rangePair :: RangePair
                     , _ei_prep      :: Maybe Text
                     , _ei_text      :: Text
                     , _ei_isClause  :: Bool
                     , _ei_isTime    :: Bool
                     }
                deriving Show

makeLenses ''EntityInfo


eiRangeID :: EntityInfo -> Range
eiRangeID e = e^.ei_rangePair.rp_full


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


emptyDPInfo :: DPInfo
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
