{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module SRL.Analyze.Type where

import           Control.DeepSeq               ( NFData)
import           Control.Lens
import           Data.Aeson                    ( ToJSON, FromJSON )
import           Data.Binary                   ( Binary )
import           Data.Hashable                 ( Hashable )
import           Data.HashMap.Strict           ( HashMap )
import           Data.IntMap                   ( IntMap )
import qualified Data.IntMap as IM
import           Data.Text                     ( Text )
import           Data.Tree                     ( Forest )
import           GHC.Generics                  ( Generic )
------ other language-engine
import           Data.Range                    ( Range )
import           FrameNet.Query.Frame          ( FrameDB )
import           Lexicon.Type                  ( ArgPattern
                                               , FNFrame
                                               , FNFrameElement
                                               , GRel
                                               , RoleInstance
                                               , RolePattInstance
                                               , SenseID
                                               )
import           NER.Type                      ( CompanyInfo )
import           NLP.Syntax.Type.Verb          ( VerbProperty(..))
import           NLP.Syntax.Type.XBar          ( Zipper,X'Tree,PreAnalysis,MarkType(..),Phase(..))
import           NLP.Type.CoreNLP              ( Dependency,Sentence,SentenceIndex,Token)
import           NLP.Type.PennTreebankII       ( Lemma,PennTree)
import           NLP.Type.SyntaxProperty       ( Voice)
import           NLP.Type.TagPos               ( CharIdx,SentItem,TagPos,TokIdx)
import           OntoNotes.Type.SenseInventory ( Inventory)
import           WikiEL.Type                   ( EntityMention, NETagger )
import           WordNet.Query                 ( WordNetDB)
------
import           SRL.Analyze.Type.Match        ( ONSenseFrameNetInstance )



data SRLData = SRLData { _srldata_sensemap  :: HashMap Text Inventory
                       , _srldata_sensestat :: HashMap (Text,Text) Int
                       , _srldata_framedb   :: FrameDB
                       , _srldata_ontomap   :: HashMap Text [(Text,FNFrame)]
                       , _srldata_rolemap   :: [RoleInstance]
                       , _srldata_subcats   :: [RolePattInstance Voice]
                       , _srldata_wordnet   :: WordNetDB
                       , _srldata_idioms    :: HashMap SenseID [[Text]]
                       }

makeLenses ''SRLData


data VerbStructure = VerbStructure { _vs_vp           :: VerbProperty (Zipper '[Lemma])
                                   , _vs_senses       :: [((ONSenseFrameNetInstance,Int),[Text])] -- rather obsolete
                                   , _vs_roleTopPatts :: [(([Text],RoleInstance,Int), [(ArgPattern () GRel, Int)])]
                                   }

makeLenses ''VerbStructure


data SentStructure = SentStructure { _ss_i              :: Int
                                   , _ss_ptr            :: PennTree
                                   , _ss_vps            :: [VerbProperty (Zipper '[Lemma])]
                                   , _ss_x'trs          :: [X'Tree 'PH1]
                                   , _ss_tagged_full    :: [TagPos TokIdx (Either (EntityMention Text) (Char,Maybe Text), MarkType)]
                                   , _ss_tagged         :: PreAnalysis '[Lemma]
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
                                         }
                      deriving (Show, Generic, ToJSON, FromJSON, NFData, Binary)

makeLenses ''DocAnalysisInput

data PredicateInfo = PredVerb { _pi_lemmas :: [Text]               -- ^ for idiom
                              , _pi_sense :: Maybe (SenseID,Bool)  -- ^ (ON sense ID, causation)
                              , _pi_verb_token_range :: Range
                              , _pi_verb  :: VerbProperty Text
                              }
                   | PredPrep { _pi_prep :: Text }
                   | PredNominalized { pi_noun :: Lemma
                                     , pi_noun_token_range :: Range
                                     , pi_nominalized :: Lemma }
                   | PredAppos
                   deriving (Generic, Show)

makePrisms ''PredicateInfo

instance FromJSON PredicateInfo
instance ToJSON PredicateInfo
instance Binary PredicateInfo
instance NFData PredicateInfo


data VertexID = RegularRange Range
              | InnerDPRange Range
              | VertexPrep Int
              | VertexPRO Int
              deriving (Generic,Show,Eq)

makePrisms ''VertexID

instance FromJSON VertexID
instance ToJSON VertexID
instance Binary VertexID
instance NFData VertexID
instance Hashable VertexID

vidToRange :: VertexID -> Maybe Range
vidToRange (RegularRange rng) = Just rng
vidToRange (InnerDPRange rng) = Just rng
vidToRange (VertexPrep _)     = Nothing
vidToRange (VertexPRO _)      = Nothing

toReg :: Range -> VertexID
toReg rng = RegularRange rng

data VertexMap = VertexMap { _vm_rangeToIndex :: HashMap VertexID Int
                           , _vm_rangeDependency :: [(Range,Range)]
                           , _vm_headRangeToFullRange :: [(Range,Range)]
                           }
               deriving Show

makeLenses ''VertexMap


data MGVertex = MGEntity    { _mv_id :: Int
                            , _mv_range :: VertexID
                            , _mv_head_range :: Maybe Range
                            , _mv_text :: Text
                            , _mv_resolved_entities :: [Text]   -- resolved named entity candidates
                            }
              | MGPredicate { _mv_id    :: Int
                            , _mv_range :: VertexID
                            , _mv_frame :: FNFrame
                            , _mv_pred_info :: PredicateInfo
                            }
              deriving (Generic, Show)

instance Binary MGVertex
instance NFData MGVertex


mv_id :: Simple Lens MGVertex Int
mv_id = lens _mv_id (\f a -> f { _mv_id = a })

mv_range :: Simple Lens MGVertex VertexID
mv_range = lens _mv_range (\f a -> f { _mv_range = a })


makePrisms ''MGVertex

isEntity :: MGVertex -> Bool
isEntity x = case x of
               MGEntity {..} -> True
               _             -> False


-- orphan
deriving instance Generic (VerbProperty Text)
instance ToJSON (VerbProperty Text)
instance FromJSON (VerbProperty Text)
instance Binary (VerbProperty Text)
instance NFData (VerbProperty Text)


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
instance Binary MGEdge
instance NFData MGEdge

data MeaningGraph = MeaningGraph { _mg_vertices :: [MGVertex]
                                 , _mg_edges :: [MGEdge]
                                 }
                  deriving (Generic, Show)

makeLenses ''MeaningGraph

instance ToJSON MeaningGraph
instance FromJSON MeaningGraph
instance Binary MeaningGraph
instance NFData MeaningGraph


data ConsoleOutput = ConsoleOutput { _outputX'tree :: Text
                                   , _outputDocStructure :: Text
                                   , _outputMatchedFrames :: Text
                                   }
                   deriving (Show,Generic)

makeLenses ''ConsoleOutput

instance ToJSON   ConsoleOutput
instance FromJSON ConsoleOutput
instance Binary   ConsoleOutput
instance NFData   ConsoleOutput

-- | Company information map
data CompanyMap =
  CompanyMap { _cmap_forest :: Forest (Either Int Text)
             , _cmap_map    :: IntMap CompanyInfo
             }

makeLenses ''CompanyMap

emptyCompanyMap :: CompanyMap
emptyCompanyMap = CompanyMap [] IM.empty


-- | SRLData + NER Data
data AnalysisData =
  AnalysisData { _analysis_SRLData    :: SRLData
               , _analysis_NETagger   :: NETagger
               , _analysis_CompanyMap :: CompanyMap
               }

makeLenses ''AnalysisData
