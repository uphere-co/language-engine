{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.Sense where

import           Control.Lens                  ((^.),(^..),_1,_2)
import           Control.Monad                 (guard)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Function                 (on)
import           Data.List                     (find,isPrefixOf,sortBy)
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
import           SRL.Analyze.Type              (SRLData(..)
                                               ,srldata_framedb,srldata_idioms
                                               ,srldata_ontomap,srldata_rolemap,srldata_sensemap
                                               ,srldata_sensestat,srldata_subcats
                                               )
import           SRL.Analyze.Type.Match        (ONSenseFrameNetInstance(..),ExceptionalFrame(..),TextifiedFrame(..)
                                               ,onfn_senseID
                                               )
--


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
                           -> ((ONSenseFrameNetInstance,Int),[Text])
                           -> [(([Text],RoleInstance,Int), [(ArgPattern () GRel,Int)])]
getTopPatternsFromONFNInst rolemap subcats ((inst,n),idiom) = do
  let sid = inst^.onfn_senseID
  rm <- filter (\rm -> rm^._1 == sid) rolemap
  let subcats' = maybeToList (find ((== sid) . (^._1)) subcats)
      toppatts_cut = cutHistogram thresholdPattStat . constructTopPatterns . (^._2) =<< subcats'
  return ((idiom,rm,n),toppatts_cut)


getVerbSenses :: SRLData
              -> (Lemma,[Lemma])
              -> ([((ONSenseFrameNetInstance,Int),[Text])]
                 ,[(([Text],RoleInstance,Int), [(ArgPattern () GRel, Int)])])
getVerbSenses sdata (lma,lmas) =
  let sensemap  = sdata^.srldata_sensemap
      sensestat = sdata^.srldata_sensestat
      framedb   = sdata^.srldata_framedb
      ontomap   = sdata^.srldata_ontomap
      rolemap   = sdata^.srldata_rolemap
      subcats   = sdata^.srldata_subcats
      idioms    = sdata^.srldata_idioms
      senses    = getSenses lma sensemap sensestat framedb ontomap
      matched_idioms = do sid <- senses^..traverse._1.onfn_senseID
                          idmlst <- maybeToList (HM.lookup sid idioms)
                          idm <- idmlst
                          guard (idm `isPrefixOf` (map unLemma lmas))
                          return (sid,idm)
      matched_senses = let match1 = do
                             sense <- senses
                             (_,idm) <- filter (\x -> (sense^._1.onfn_senseID == x^._1)) matched_idioms
                             return (sense,idm)
                       in if null match1 then map (\x -> (x,[unLemma lma])) senses else match1
      rmtoppatts = do inst <- sortBy (flip compare `on` (^._2)) matched_senses
                      getTopPatternsFromONFNInst rolemap subcats inst
  in (matched_senses,rmtoppatts)
