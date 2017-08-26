{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.App.Analyze.Type where

import           Control.Lens                  ((^.),_2,makeLenses)
import           Data.Function                 (on)
import           Data.HashMap.Strict           (HashMap)
import           Data.List                     (maximumBy)
import           Data.Text                     (Text)
--
import           FrameNet.Query.Frame          (FrameDB)
import           Lexicon.Type                  (ArgPattern,GRel,RoleInstance,RolePattInstance,SenseID)
import           NLP.Syntax.Type               (BitreeZipperICP,VerbProperty(..))
import           NLP.Type.PennTreebankII       (Lemma)
import           NLP.Type.SyntaxProperty       (Voice)
--
import           OntoNotes.Type.SenseInventory (Inventory)


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



chooseFrame :: [(ONSenseFrameNetInstance,Int)] -> Maybe (ONSenseFrameNetInstance,Int)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._2)) xs)



data AnalyzePredata = AnalyzePredata { _analyze_sensemap  :: HashMap Text Inventory
                                     , _analyze_sensestat :: HashMap (Text,Text) Int
                                     , _analyze_framedb   :: FrameDB
                                     , _analyze_ontomap   :: HashMap Text [(Text,Text)]
                                     , _analyze_rolemap   :: [RoleInstance]
                                     , _analyze_subcats   :: [RolePattInstance Voice]
                                     }

makeLenses ''AnalyzePredata


data VerbStructure = VerbStructure { _vs_vp           :: VerbProperty (BitreeZipperICP '[Lemma])
                                   , _vs_lma          :: Text
                                   , _vs_senses       :: [(ONSenseFrameNetInstance,Int)]
                                   , _vs_mrmmtoppatts :: Maybe (RoleInstance, Maybe [(ArgPattern () GRel, Int)])
                                   }

makeLenses ''VerbStructure                     

{- 
data DocStructure = DocStructure



([[Maybe Token]], [SentItem CharIdx],[TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
  ,[Maybe (Int, PennTree, [NLP.Syntax.Type.VerbProperty (NLP.Syntax.Type.BitreeZipperICP '[Lemma])]
          , NLP.Syntax.Type.ClauseTree, Maybe [Bitree (Range,CP) (Range,CP),                                                                                                                                                                                                                                                                                                                         [(NLP.Syntax.Type.VerbProperty
-}
