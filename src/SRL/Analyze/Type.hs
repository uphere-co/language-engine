{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module SRL.Analyze.Type where

import           Control.Lens                  ((^.),_2,makeLenses,makePrisms)
import           Data.Aeson
import           Data.Function                 (on)
import           Data.HashMap.Strict           (HashMap)
import           Data.List                     (maximumBy)
import           Data.Text                     (Text)
import           GHC.Generics
--
import           Data.Bitree                   (Bitree)
import           Data.Range                    (Range)
import           FrameNet.Query.Frame          (FrameDB)
import           Lexicon.Type                  (ArgPattern,GRel,RoleInstance,RolePattInstance,SenseID)
import           NLP.Syntax.Type               (ClauseTree,MarkType(..))
import           NLP.Syntax.Type.Verb          (VerbProperty(..))
import           NLP.Syntax.Type.XBar          (Zipper,CPDP,DetP)
import           NLP.Type.CoreNLP              (Dependency,Sentence,SentenceIndex,Token)
import           NLP.Type.PennTreebankII       (Lemma,PennTree)
import           NLP.Type.SyntaxProperty       (Voice)
import           NLP.Type.TagPos               (CharIdx,SentItem,TagPos,TokIdx)
import           WikiEL.EntityLinking          (EntityMention)
--
import           OntoNotes.Type.SenseInventory (Inventory)
--


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
                                     , _analyze_ontomap   :: HashMap Text [(Text,Text)]
                                     , _analyze_rolemap   :: [RoleInstance]
                                     , _analyze_subcats   :: [RolePattInstance Voice]
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
                                   , _ss_clausetr       :: ClauseTree
                                   , _ss_cpstr          :: [Bitree (Range,CPDP '[Lemma]) (Range,CPDP '[Lemma])]
                                   , _ss_tagged         :: [TagPos TokIdx MarkType]
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



data AdjustedDetP = WithPrep { _adj_prep :: Text
                             , _adj_pp   :: Zipper '[Lemma]
                             , _adj_dp   :: DetP '[Lemma]
                             }
                  | WithoutPrep { _adj_dp :: DetP '[Lemma] }

makePrisms ''AdjustedDetP

getDetP (WithPrep _ _ z) = z
getDetP (WithoutPrep z ) = z




data MGVertex = MGEntity    { _mv_id :: Int
                            , _mv_range :: Range
                            , _mv_text :: Text
                            , _mv_resolved_entities :: [Text]   -- resolved named entity candidates
                            }
              | MGPredicate { _mv_id    :: Int
                            , _mv_range :: Range
                            , _mv_frame :: Text
                            , _mv_sense :: SenseID
                            , _mv_verb  :: VerbProperty Text -- (Text,Tense,Aspect,Voice,Maybe Text)
                            }
              | MGNominalPredicate { _mv_id    :: Int
                                   , _mv_range :: Range
                                   , _mv_frame :: Text
                                   -- , _mv_verb  :: VerbProperty Text -- (Text,Tense,Aspect,Voice,Maybe Text)
                                   }

              deriving (Generic, Show)

makeLenses ''MGVertex
makePrisms ''MGVertex

-- orphan
deriving instance Generic (VerbProperty Text)

-- orphan
instance ToJSON (VerbProperty Text)

-- orphan
instance FromJSON (VerbProperty Text)


instance ToJSON MGVertex
instance FromJSON MGVertex

data MGEdge = MGEdge { _me_relation :: Text
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
