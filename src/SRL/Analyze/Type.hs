{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module SRL.Analyze.Type where

import           Control.Lens
import           Data.Aeson
import           Data.Function                 (on)
import           Data.HashMap.Strict           (HashMap)
import           Data.List                     (maximumBy)
import           Data.Text                     (Text)
import           GHC.Generics
--
import           Data.Range                    (Range)
import           FrameNet.Query.Frame          (FrameDB)
import           Lexicon.Type                  (ArgPattern,FNFrame,FNFrameElement,GRel
                                               ,RoleInstance,RolePattInstance,SenseID)
import           NLP.Syntax.Type               (MarkType(..))
import           NLP.Syntax.Type.Verb          (VerbProperty(..))
import           NLP.Syntax.Type.XBar          (Zipper,X'Tree,TaggedLemma,CompVP)
import           NLP.Type.CoreNLP              (Dependency,Sentence,SentenceIndex,Token)
import           NLP.Type.PennTreebankII       (Lemma,PennTree)
import           NLP.Type.SyntaxProperty       (Voice)
import           NLP.Type.TagPos               (CharIdx,SentItem,TagPos,TokIdx)
import           WikiEL.Type                   (EntityMention)
import           WordNet.Query                 (WordNetDB)
--
import           OntoNotes.Type.SenseInventory (Inventory)
--


data EntityInfo = EI { _ei_fullRange :: Range
                     , _ei_headRange :: Range
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




data AnalyzePredata = AnalyzePredata { _analyze_sensemap  :: HashMap Text Inventory
                                     , _analyze_sensestat :: HashMap (Text,Text) Int
                                     , _analyze_framedb   :: FrameDB
                                     , _analyze_ontomap   :: HashMap Text [(Text,FNFrame)]
                                     , _analyze_rolemap   :: [RoleInstance]
                                     , _analyze_subcats   :: [RolePattInstance Voice]
                                     , _analyze_wordnet   :: WordNetDB
                                     }

makeLenses ''AnalyzePredata


data VerbStructure = VerbStructure { _vs_vp           :: VerbProperty (Zipper '[Lemma])
                                   , _vs_senses       :: [(ONSenseFrameNetInstance,Int)]
                                   , _vs_roleTopPatts :: [((RoleInstance,Int), [(ArgPattern () GRel, Int)])]
                                   }

makeLenses ''VerbStructure


data SentStructure = SentStructure { _ss_i              :: Int
                                   , _ss_ptr            :: PennTree
                                   , _ss_vps            :: [VerbProperty (Zipper '[Lemma])]
                                   , _ss_x'tr           :: [X'Tree '[Lemma]]
                                   , _ss_tagged_full    :: [TagPos TokIdx (Either (EntityMention Text) (Char,Maybe Text), MarkType)]
                                   , _ss_tagged         :: TaggedLemma '[Lemma]
                                   , _ss_verbStructures :: [VerbStructure]
                                   }

makeLenses ''SentStructure

data DocStructure = DocStructure { _ds_mtokenss :: [[Maybe Token]]
                                 , _ds_sentitems :: [SentItem CharIdx]
                                 , _ds_mergedtags :: [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
                                 , _ds_sentStructures :: [Maybe SentStructure]
                                 }

makeLenses ''DocStructure

data DocAnalysisInput = DocAnalysisInput { _dainput_sents     :: [Sentence]
                                         , _dainput_sentidxs  :: [Maybe SentenceIndex]
                                         , _dainput_sentitems :: [SentItem CharIdx]
                                         , _dainput_tokss     :: [[Token]]
                                         , _dainput_mptrs     :: [Maybe PennTree]
                                         , _dainput_deps      :: [Dependency]
                                         , _dainput_mtmxs     :: Maybe [TagPos TokIdx (Maybe Text)]
                                         } deriving (Show, Generic)

makeLenses ''DocAnalysisInput

instance ToJSON DocAnalysisInput where
  toJSON = genericToJSON defaultOptions

instance FromJSON DocAnalysisInput where
  parseJSON = genericParseJSON defaultOptions






data VertexMap = VertexMap { _vm_rangeToIndex :: HashMap (Int,Maybe Range) Int
                           , _vm_rangeDependency :: [(Range,Range)]
                           , _vm_headRangeToFullRange :: [(Range,Range)]
                           }
               deriving Show

makeLenses ''VertexMap


data PredicateInfo = PredVerb { _pi_sense :: (SenseID,Bool)  -- ^ (ON sense ID, causation)
                              , _pi_verb  :: VerbProperty Text
                              }
                   | PredPrep { _pi_prep :: Text }
                   | PredNominalized { pi_noun :: Lemma
                                     , pi_nominalized :: Lemma }
                   | PredAppos
                   deriving (Generic, Show)

makePrisms ''PredicateInfo


data MGVertex = MGEntity    { _mv_id :: Int
                            , _mv_range :: Maybe Range
                            , _mv_head_range :: Maybe Range
                            , _mv_text :: Text
                            , _mv_resolved_entities :: [Text]   -- resolved named entity candidates
                            }
              | MGPredicate { _mv_id    :: Int
                            , _mv_range :: Maybe Range
                            , _mv_frame :: FNFrame
                            , _mv_pred_info :: PredicateInfo
                            }
              deriving (Generic, Show)


mv_id :: Simple Lens MGVertex Int
mv_id = lens _mv_id (\f a -> f { _mv_id = a })

mv_range :: Simple Lens MGVertex (Maybe Range)
mv_range = lens _mv_range (\f a -> f { _mv_range = a })


makePrisms ''MGVertex

-- orphan
deriving instance Generic (VerbProperty Text)

-- orphan
instance ToJSON (VerbProperty Text)

-- orphan
instance FromJSON (VerbProperty Text)

instance FromJSON PredicateInfo

instance ToJSON PredicateInfo

instance ToJSON MGVertex

instance FromJSON MGVertex

data MGEdge = MGEdge { _me_relation :: FNFrameElement
                     , _me_ismodifier :: Bool
                     , _me_prep :: Maybe Text
                     , _me_start :: Int
                     , _me_end :: Int }

            deriving (Generic, Show)

makeLenses ''MGEdge

instance ToJSON MGEdge
instance FromJSON MGEdge

data MeaningGraph = MeaningGraph { _mg_vertices :: [MGVertex]
                                 , _mg_edges :: [MGEdge]
                                 }
                  deriving (Generic, Show)

makeLenses ''MeaningGraph

instance ToJSON MeaningGraph
instance FromJSON MeaningGraph
