{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.Sense where

import           Control.Lens                  ((^.),(^..),_1,_2)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Function                 (on)
import           Data.List                     (find,sortBy)
import           Data.Maybe                    (fromMaybe,maybeToList)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
--
import           FrameNet.Query.Frame          (FrameDB,frameDB)
import           FrameNet.Type.Common          (CoreType(..))
import           FrameNet.Type.Frame           (fe_coreType,fe_name,frame_FE)
import           Lexicon.Merge                 (constructTopPatterns)
import           Lexicon.Query                 (cutHistogram)
import           Lexicon.Type                  (FNFrame(..),RoleInstance,RolePattInstance,ArgPattern,GRel,POSVorN(..))
import           NLP.Type.PennTreebankII       (Lemma(..))
import           NLP.Type.SyntaxProperty       (Voice)
import           OntoNotes.Type.SenseInventory (Inventory(..),sense_group,sense_n,sense_name,inventory_senses)
--
import           SRL.Analyze.Parameter         (thresholdPattStat)
import           SRL.Analyze.Type              (ONSenseFrameNetInstance,AnalyzePredata(..),ExceptionalFrame(..)
                                               ,ONSenseFrameNetInstance(..),TextifiedFrame(..)
                                               ,analyze_framedb
                                               ,analyze_ontomap,analyze_rolemap,analyze_sensemap
                                               ,analyze_sensestat,analyze_subcats
                                               ,onfn_senseID
                                               )

getSenses :: Lemma
          -> HashMap Text Inventory
          -> HashMap (Text,Text) Int
          -> FrameDB
          -> HashMap Text [(Text,FNFrame)]
          -> [(ONSenseFrameNetInstance,Int)]
getSenses (Lemma lma) sensemap sensestat framedb ontomap = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let sid = (lma,Verb, s^.sense_group <> "." <> s^.sense_n)
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = s^.sense_name
      tframe = fromMaybe (Left FrameNone) $ do
        lst <- HM.lookup lma ontomap
        frtxt <- unFNFrame <$> lookup (s^.sense_group <> "." <> s^.sense_n) lst
        case frtxt of
          "copula"    -> return (Left FrameCopula)
          "idioms"    -> return (Left FrameIdiom)
          "lightverb" -> return (Left FrameLightVerb)
          _ -> do
            frame <- HM.lookup frtxt (framedb^.frameDB)
            let fes = frame^..frame_FE.traverse
                corefes = map (^.fe_name)
                        . filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed)
                        $ fes
                perifes = map (^.fe_name)
                        . filter (\fe -> fe^.fe_coreType == Peripheral)
                        $ fes
            return (Right (TF frtxt corefes perifes))
  return ((ONFNInstance sid txt_def tframe),num)


getTopPatternsFromONFNInst :: [RoleInstance]
                           -> [RolePattInstance Voice]
                           -> (ONSenseFrameNetInstance,Int)
                           -> [((RoleInstance,Int), [(ArgPattern () GRel,Int)])]
getTopPatternsFromONFNInst rolemap subcats (inst,n) = do
  let sid = inst^.onfn_senseID
  rm <- filter (\rm -> rm^._1 == sid) rolemap
  let subcats' = maybeToList (find ((== sid) . (^._1)) subcats)
      toppatts_cut = cutHistogram thresholdPattStat . constructTopPatterns . (^._2) =<< subcats'
  return ((rm,n),toppatts_cut)


getVerbSenses :: AnalyzePredata -> Lemma -> ([(ONSenseFrameNetInstance,Int)],[((RoleInstance,Int), [(ArgPattern () GRel, Int)])])
getVerbSenses apredata lma = 
  let sensemap = apredata^.analyze_sensemap
      sensestat = apredata^.analyze_sensestat
      framedb = apredata^.analyze_framedb
      ontomap = apredata^.analyze_ontomap
      rolemap = apredata^.analyze_rolemap
      subcats = apredata^.analyze_subcats
      senses = getSenses lma sensemap sensestat framedb ontomap
      rmtoppatts = do inst <- sortBy (flip compare `on` (^._2)) senses
                      getTopPatternsFromONFNInst rolemap subcats inst
  in (senses,rmtoppatts)
