{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module SRL.Analyze.Format where

import           Control.Lens                            ((^..),(^.),(^?),_1,_2,_3,_Just,_Right,to)
import           Control.Lens.Extras                     (is)
import           Data.Foldable
import           Data.List                               (intersperse)
import           Data.Maybe                              (fromMaybe,mapMaybe)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Format                as T.F
import qualified Data.Text.Lazy                  as T.L
import qualified Data.Text.IO                    as T.IO
import           Formatting                              (Format,(%),(%.),sformat,stext,int)
import qualified Formatting                      as F    (left,right)
import           Text.PrettyPrint.Boxes                  (Box,left,hsep,text,top,vcat,render)
--
import           CoreNLP.Simple.Convert                  (sentToTokens')
import           FrameNet.Query.Frame                    (FrameDB)
import qualified HTMLEntities.Text             as HTMLT
import           Lexicon.Format                          (formatArgPattStat,formatRoleMap)
import           Lexicon.Type                            (ArgPattern(..),RoleInstance,GRel(..)
                                                         ,FNFrame(..),FNFrameElement(..))
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree,prettyPrint)
import           NLP.Syntax.Type.Resolve                 (Referent(..),referent2CompVP)
import           NLP.Syntax.Type.Verb                    (vp_aspect,vp_auxiliary,vp_lemma,vp_negation,vp_tense)
import           NLP.Syntax.Type.XBar                    (CompVP(..),PreAnalysis
                                                         ,CP,X'Tree,Phase(..),SPhase(..))
import           NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos                         (CharIdx,TokIdx,TagPos(..),SentItem)
import           WikiEL.Type                             (EMInfo,EntityMention,PreNE,UIDCite(..),_emuid,entityName,resolvedUID)
--
import           SRL.Analyze.Match.Frame                 (matchFrame)
import           SRL.Analyze.Type                        (DocStructure(..),SentStructure(..),VerbStructure(..)
                                                         ,MGEdge(..),MGVertex(..),MeaningGraph
                                                         ,PredicateInfo(..)
                                                         ,_MGPredicate,_MGEntity
                                                         ,mg_vertices,mg_edges
                                                         ,me_relation,me_ismodifier,me_prep,me_start,me_end
                                                         ,ss_x'trs)
import           SRL.Analyze.Type.Match                  (ExceptionalFrame(..),ONSenseFrameNetInstance(..),FrameMatchResult(..)
                                                         ,onfn_senseID,onfn_definition,onfn_frame
                                                         ,tf_frameID,tf_feCore,tf_fePeri)
import           SRL.Analyze.Util                        (addTag,convertTagPosFromTokenToChar,underlineText)

-- note that left/right are reversed
rs,ls :: Int -> Format r (Text -> r)
rs n = F.left  n ' ' %. stext
ls n = F.right n ' ' %. stext

rd,ld :: (Integral a) => Int -> Format r (a -> r)
rd n = F.left  n ' ' %. int
ld n = F.right n ' ' %. int

formatExFrame :: ExceptionalFrame -> Text
formatExFrame FrameCopula = "** COPULA **"
formatExFrame FrameIdiom  = "** IDIOM **"
formatExFrame FrameLightVerb = "** LIGHT VERB **"
formatExFrame _              = "** UNIDENTIFIED **"


formatLemmaPOS :: Token -> Text
formatLemmaPOS t =
  sformat (rs 10 % " " % rs 5) (t^.token_lemma) (T.pack (show (t^.token_pos)))


formatTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> [Text]
formatTimex (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]


showTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> IO ()
showTimex (s,a) = T.IO.putStrLn (T.intercalate "\n" (formatTimex (s,a)))



getFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> [Text]
getFormatTimex' (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> IO ()
showFormatTimex' (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex' (s,a)))




formatSense :: ((ONSenseFrameNetInstance,Int),a) -> Text
formatSense ((onfninst,num),_) =
  sformat (ls 8 % " (" % rd 4 % " cases) | " % ls 40 % " | " % ls 20 % " | " % ls 40 % "      ------       " % ls 30 % " ")
    (onfninst^.onfn_senseID._3)
    num
    (onfninst^.onfn_definition)
    (onfninst^.onfn_frame.to (either formatExFrame (^.tf_frameID)))
    (maybe "" (T.intercalate ", ") ((onfninst^?onfn_frame._Right.tf_feCore)))
    (maybe "" (T.intercalate ", ") ((onfninst^?onfn_frame._Right.tf_fePeri)))



formatFrame :: (ONSenseFrameNetInstance, Int) -> Text
formatFrame t =
  sformat (" " % ls 20 % " " % rd 4 % " | " % ls 40 % "      ------      " % ls 30 % "\n")
    (t ^. _1 . onfn_frame . to (either formatExFrame (^.tf_frameID)))
    (t ^. _2)
    (maybe "" (T.intercalate ", ") (t^?_1.onfn_frame._Right.tf_feCore))
    (maybe "" (T.intercalate ", ") (t^?_1.onfn_frame._Right.tf_fePeri))


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [((ONSenseFrameNetInstance,Int),[Text])]
             -> [(([Text],RoleInstance,Int), [(ArgPattern () GRel,Int)])]
             -> Text
formatSenses doesShowOtherSense onfnlst rmtoppatts
  =  "--------------------------------------------------------------------------------------------------\n"
     <> T.intercalate "\n--------------------------------------------------------------------------------------------------\n" (flip map rmtoppatts (\((_,rm,_),toppatts) ->
          let argpattstr = formatArgPattStat toppatts
              mfrm = find (\x -> x^._1._1.onfn_senseID == rm^._1 ) onfnlst
              framestr = case mfrm of
                           Nothing -> "Frames: "
                           Just (frm,idiom) -> T.pack (show idiom) <> " " <> "Frames: " <> formatFrame frm
          in framestr <>
             (formatRoleMap (rm^._2) <> ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<> argpattstr))))
     <> "\n--------------------------------------------------------------------------------------------------\n"
     <> if doesShowOtherSense
        then "\n\n\n*********************************************\n" <> T.unlines (map formatSense onfnlst)
        else ""


formatLinkedMention :: EntityMention Text -> Text
formatLinkedMention Cite {..} = sformat (rd 3 % ": (-> " % rd 3 % ") " % stext % " ") (_emuid _uid) (_emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = sformat (rd 3 % ":          "% stext % " ")           (_emuid _uid)               (formatEMInfo _info)


formatIndexedTimex :: (Char,Maybe Text) -> Text
formatIndexedTimex (c,mtxt)   = sformat (rs 3 % ":          " % stext % " ") (T.singleton c) (fromMaybe "" mtxt)


formatTaggedSentences :: (a -> Text)
                      ->  [(SentItem CharIdx,[TagPos CharIdx a])]
                      -> Box
formatTaggedSentences f sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText f (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map text txts


formatPreNE :: PreNE -> Text
formatPreNE tag = case resolvedUID tag of
                    Left _ -> "unresolved"
                    Right i -> T.pack (show i)


formatEMInfo :: EMInfo Text -> Text
formatEMInfo em@(_,_,tag) = sformat (ls 25 % " " % ls 20) (entityName em) (formatPreNE tag)


formatTagged :: [[Maybe Token]]
             -> [SentItem CharIdx]
             -> [TagPos TokIdx (Either (EntityMention Text) (Char,(Maybe Text)))]
             -> Box
formatTagged mtokenss sentitems tlst =
  let toks = concatMap (map snd . sentToTokens') mtokenss
      clst = mapMaybe (convertTagPosFromTokenToChar toks) tlst
      sents_tagged = map (addTag clst) sentitems
      doc1 = formatTaggedSentences (either (T.pack . show . _emuid . _uid) (T.singleton . (^._1))) sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text . either formatLinkedMention formatIndexedTimex) $ map (\(TagPos (_,_,x)) -> x) clst
  in hsep 10 left [doc1,doc2]




formatDocStructure :: Bool -> DocStructure -> [Text]
formatDocStructure showdetail (DocStructure mtokenss sentitems mergedtags sstrs) =
  let line1 = [ "=================================================================================================="
              , "-- Time and NER tagged text ----------------------------------------------------------------------"
              , render (formatTagged mtokenss sentitems mergedtags)
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]
      line3 = concatMap (maybe [""] (formatSentStructure showdetail)) sstrs

  in line1  ++ line3


formatSentStructure :: Bool -> SentStructure -> [Text]
formatSentStructure _showdetail ss@(SentStructure i ptr _ _ _ _ vstrs) =
   let subline1 = [ sformat ("-- Sentence " % rd 3 % " ----------------------------------------------------------------------------------") i
                  , formatIndexTokensFromTree 0 ptr
                  , prettyPrint 0 ptr ]

       subline1_1 = [ "--------------------------------------------------------------------------------------------------" ] ++
                    map (formatX'Tree SPH1) (ss^.ss_x'trs) ++
                    [ "=================================================================================================" ]
       subline2 = map formatVerbStructure vstrs
   in subline1 ++ subline1_1 ++ concat subline2


formatVerbStructure :: VerbStructure -> [Text]
formatVerbStructure (VerbStructure vp senses mrmmtoppatts) =
  [--  formatVPwithPAWS tagged clausetr x'tr vp
    sformat ("Verb: " % ls 20) (vp^.vp_lemma.to unLemma)
  , formatSenses False senses mrmmtoppatts
  ]


showMatchedFE :: PreAnalysis '[Lemma] -> (FNFrameElement,Referent (CompVP 'PH1)) -> Text
showMatchedFE _a0 (fe,x) =
  case referent2CompVP x of
    --                              FE         range         prep          text
    CompVP_DP rng_dp -> sformat (ls 15 % ": " % ls 7 % " " % rs 3 % " " % stext) (unFNFrameElement fe) (T.pack ("DP" ++ show rng_dp)) "" ""
    CompVP_AP rng_ap -> sformat (ls 15 % ": " % ls 7 % " " % rs 3 % " " % stext) (unFNFrameElement fe) (T.pack ("AP" ++ show rng_ap)) "" ""
    CompVP_CP rng_cp -> sformat (ls 15 % ": " % ls 7 % " " % rs 3 % " " % stext) (unFNFrameElement fe) (T.pack ("CP" ++ show rng_cp)) "" ""
    CompVP_PP rng_pp -> sformat (ls 15 % ": " % ls 7 % " " % rs 3 % " " % stext) (unFNFrameElement fe) (T.pack ("PP" ++ show rng_pp)) "" ""


showMatchedFrame :: FrameDB
                 -> PreAnalysis '[Lemma]
                 -> (X'Tree 'PH1,VerbStructure, CP 'PH1)
                 -> [Text]
showMatchedFrame framedb tagged (x'tr,vstr,cp) =
  let x = "---------------------------"
      xs = flip concatMap (matchFrame framedb x'tr (vstr,cp)) $ \(rng,_,_,FMR idiom frame mselected _,_) ->
             ([ "predicate: "  <> T.pack (show rng)
              , "Verb: "  <> T.intercalate " " idiom
              , "Frame: " <> unFNFrame frame ]
              ++ (flip concatMap mselected (\(_,felst) -> map (showMatchedFE tagged) felst)))
  in x:xs



formatMGEdge :: MGEdge -> Text
formatMGEdge e = format "i{} -> i{} [label=\"{}\" style=\"{}\" fontsize=12.0 {}];"
                   (e^.me_start
                   ,e^.me_end
                   ,unFNFrameElement (e^.me_relation) <> maybe "" (":" <>) (e^.me_prep) -- <> maybe "" (":" <>) (mkPROText =<< e^.me_eci)
                   ,if e^.me_relation == "ref" || e^.me_relation == "PRO"
                    then "dotted"
                    else if (e^.me_ismodifier)
                         then "bold"
                         else "solid" :: Text
                   ,"" :: Text -- if e^.me_relation == "ref" then "constraint=false" else "" :: Text
                   )
                 <>
                 if (e^.me_ismodifier)
                 then format "\n  {rankdir=TB; i{} -> i{} [style=invis]};"
                        (e^.me_end,e^.me_start)
                 else ""
  where
    format fmt ps = T.L.toStrict (T.F.format fmt ps)


formatMGVertex :: MGVertex -> (Int,Text)
formatMGVertex (MGPredicate i _ f (PredVerb idiom _ _ v))
  = (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
        "<tr><td colspan=\"4\">" <> unFNFrame f <> "</td></tr>" <>
        "<tr>" <>
        "<td width=\"20\">" <> T.intercalate " " (v^..vp_auxiliary.traverse._1)   <> "</td>" <>
        "<td width=\"20\">" <> fromMaybe "" (v^?vp_negation._Just._1)             <> "</td>" <>
        "<td>" <> T.intercalate " " idiom {- v^.vp_lemma.to unLemma -}            <> "</td>" <>
        "<td>" <> formatTense (v^.vp_tense) <> "." <> formatAspect (v^.vp_aspect) <> "</td>" <>
        "</tr>" <>
        "</table>" )
formatMGVertex (MGPredicate i _ f (PredPrep p))
  = (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
        "<tr><td colspan=\"4\">" <> unFNFrame f <> "</td></tr>" <>
        "<tr>" <>
        "<td width=\"20\">" <>          " </td>" <>
        "<td width=\"20\">" <>          " </td>" <>
        "<td>"              <> p      <> "</td>" <>
        "<td>"              <> "prep" <> "</td>" <>
        "</tr>" <>
        "</table>" )
formatMGVertex (MGPredicate i _ f (PredNominalized n _ _))
  = (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
        "<tr><td colspan=\"4\">" <> unFNFrame f <> "</td></tr>" <>
        "<tr>" <>
        "<td width=\"20\">" <> " </td>" <>
        "<td width=\"20\">" <> " </td>" <>
        "<td>"              <> unLemma n <> " </td>" <>
        "<td> Nom.Deverb </td>" <>
        "</tr>" <>
        "</table>" )
formatMGVertex (MGPredicate i _ f PredAppos)
  = (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
        "<tr><td colspan=\"4\">" <> unFNFrame f <> "</td></tr>" <>
        "<tr>" <>
        "<td width=\"20\">" <> " </td>" <>
        "<td width=\"20\">" <> " </td>" <>
        "<td> </td>" <>
        "<td> Nom.Appos </td>" <>
        "</tr>" <>
        "</table>" )
formatMGVertex (MGEntity i _ _ t ns)
  = (i,"<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
       "<tr><td>" <> (HTMLT.text t) <> "</td></tr>" <>
       T.concat (map (\x -> "<tr><td>"<> (HTMLT.text x) <>"</td></tr>") ns) <>
       "</table>")




dotMeaningGraph :: Maybe Text -> MeaningGraph -> Text
dotMeaningGraph mtitle mg = format "digraph G {\n  {}\n  {}\n  {}\n}" (vtxt,etxt,ttxt)
  where
    format fmt ps = T.L.toStrict (T.F.format fmt ps)
    vtxt = let vertices = mg^.mg_vertices
               verbs = (map formatMGVertex . filter (is _MGPredicate)) vertices
               entities = (map formatMGVertex . filter (is _MGEntity)) vertices

           in (T.intercalate "\n  " . map (\(i,t) -> format "i{} [shape=plaintext, margin=0, style=filled, fillcolor=grey label=<{}>];" (i,t))) verbs <>  "\n  " <>
              (T.intercalate "\n  " . map (\(i,t) -> format "i{} [shape=plaintext, margin=0, label=<{}>];" (i,t))) entities
    --
    etxt = let edges = mg^.mg_edges in (T.intercalate "\n  " . map formatMGEdge) edges
    --
    ttxt = maybe "" (\title -> "labelloc=\"t\"; \n " <> "label=\"" <> title <> "\"; \n ") mtitle
