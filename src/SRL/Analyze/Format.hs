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
import           Data.List                               (intercalate,intersperse)
import           Data.Maybe                              (fromMaybe,mapMaybe)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Format                as T.F
import qualified Data.Text.Lazy                  as T.L
import qualified Data.Text.IO                    as T.IO

import           Text.PrettyPrint.Boxes                  (Box,left,hsep,text,top,vcat,render)
import           Text.Printf                             (printf)
--
import           CoreNLP.Simple.Convert                  (sentToTokens')
import           Data.BitreeZipper
import           FrameNet.Query.Frame                    (FrameDB)
import qualified HTMLEntities.Text             as HTMLT
import           Lexicon.Format                          (formatArgPattStat,formatRoleMap)
import           Lexicon.Type                            (ArgPattern(..),RoleInstance,GRel(..)
                                                         ,FNFrame(..),FNFrameElement(..))
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree,prettyPrint)
import           NLP.Syntax.Type.Verb                    (vp_aspect,vp_auxiliary,vp_lemma,vp_negation,vp_tense)
import           NLP.Syntax.Type.XBar                    (CompVP(..),CompPP(..),Prep(..),PrepClass(..),PreAnalysis
                                                         ,CP,X'Tree,Phase(..),SPhase(..)
                                                         ,headTextDP,headX,complement,maximalProjection
                                                         ,tokensByRange
                                                         ,hp_prep,hp_pclass)
import           NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos                         (CharIdx,TokIdx,TagPos(..),SentItem)
import qualified WikiEL                        as WEL
import           WikiEL.Type                             (EMInfo,EntityMention,PreNE,UIDCite(..),_emuid)
import           WikiEL.WikiNamedEntityTagger            (resolvedUID)
--
import           SRL.Analyze.Match.Frame                 (matchFrame)
import           SRL.Analyze.Type                        (DocStructure(..),SentStructure(..),VerbStructure(..)
                                                         ,MGEdge(..),MGVertex(..),MeaningGraph
                                                         ,PredicateInfo(..)
                                                         ,_MGPredicate,_MGEntity
                                                         ,mg_vertices,mg_edges
                                                         ,me_relation,me_ismodifier,me_prep,me_start,me_end
                                                         ,vs_vp,ss_x'trs)
import           SRL.Analyze.Type.Match                  (ExceptionalFrame(..),ONSenseFrameNetInstance(..),FrameMatchResult(..)
                                                         ,MatchedElement
                                                         ,onfn_senseID,onfn_definition,onfn_frame
                                                         ,tf_frameID,tf_feCore,tf_fePeri)
import           SRL.Analyze.Util                        (addTag,convertTagPosFromTokenToChar,underlineText)


formatExFrame :: ExceptionalFrame -> Text
formatExFrame FrameCopula = "** COPULA **"
formatExFrame FrameIdiom  = "** IDIOM **"
formatExFrame FrameLightVerb = "** LIGHT VERB **"
formatExFrame _              = "** UNIDENTIFIED **"


formatLemmaPOS :: Token -> String
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


formatTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> [Text]
formatTimex (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]


showTimex :: (SentItem CharIdx, [TagPos CharIdx (Maybe Text)]) -> IO ()
showTimex (s,a) = T.IO.putStrLn (T.intercalate "\n" (formatTimex (s,a)))



getFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> [Text]
getFormatTimex' (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex' :: (SentItem CharIdx,[TagPos CharIdx (Maybe Text)]) -> IO ()
showFormatTimex' (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex' (s,a)))




formatSense :: ((ONSenseFrameNetInstance,Int),a) -> String
formatSense ((onfninst,num),_) =
  printf "%-8s (%4d cases) | %-40s | %-20s | %-40s      ------       %-30s "
         (onfninst^.onfn_senseID._3)
         num
         (onfninst^.onfn_definition)
         (onfninst^.onfn_frame.to (either formatExFrame (^.tf_frameID)))
         (maybe "" (T.intercalate ", ") ((onfninst^?onfn_frame._Right.tf_feCore)))
         (maybe "" (T.intercalate ", ") ((onfninst^?onfn_frame._Right.tf_fePeri)))



formatFrame :: (ONSenseFrameNetInstance, Int) -> String
formatFrame t =
  printf " %-20s %-4d | %-40s      ------      %-30s\n"
    (t ^. _1 . onfn_frame . to (either formatExFrame (^.tf_frameID)))
    (t ^. _2)
    (maybe "" (T.intercalate ", ") (t^?_1.onfn_frame._Right.tf_feCore))
    (maybe "" (T.intercalate ", ") (t^?_1.onfn_frame._Right.tf_fePeri))


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [((ONSenseFrameNetInstance,Int),[Text])]
             -> [(([Text],RoleInstance,Int), [(ArgPattern () GRel,Int)])]
             -> String
formatSenses doesShowOtherSense onfnlst rmtoppatts
  =  "--------------------------------------------------------------------------------------------------\n"
     ++ intercalate "\n--------------------------------------------------------------------------------------------------\n" (flip map rmtoppatts (\((_,rm,_),toppatts) ->
          let argpattstr = formatArgPattStat toppatts
              mfrm = find (\x -> x^._1._1.onfn_senseID == rm^._1 ) onfnlst
              framestr = case mfrm of
                           Nothing -> "Frames: "
                           Just (frm,idiom) -> show idiom ++ " " ++ "Frames: " ++ formatFrame frm
          in framestr ++
             (formatRoleMap (rm^._2) ++ ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<> argpattstr))))
     ++ "\n--------------------------------------------------------------------------------------------------\n"
     ++ if doesShowOtherSense
        then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense onfnlst)
        else ""


formatLinkedMention :: EntityMention Text -> String
formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (_emuid _uid) (_emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (_emuid _uid)               (formatEMInfo _info)


formatIndexedTimex :: (Char,Maybe Text) -> String
formatIndexedTimex (c,mtxt)   = printf "%3s:          %s " (T.singleton c) (fromMaybe "" mtxt)


formatTaggedSentences :: (a -> Text)
                      ->  [(SentItem CharIdx,[TagPos CharIdx a])]
                      -> Box
formatTaggedSentences f sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText f (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map (text . T.unpack) txts


formatPreNE :: PreNE -> String
formatPreNE tag = case resolvedUID tag of
                    Left _ -> "unresolved"
                    Right i -> show i


formatEMInfo :: EMInfo Text -> String
formatEMInfo em@(_,_,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)


formatTagged :: [[Maybe Token]]
             -> [SentItem CharIdx]
             -> [TagPos TokIdx (Either (EntityMention Text) (Char,(Maybe Text)))]
             -> Box
formatTagged mtokenss sentitems tlst =
  let toks = concatMap (map snd . sentToTokens') mtokenss
      clst = mapMaybe (convertTagPosFromTokenToChar toks) tlst
      sents_tagged = map (addTag clst) sentitems
      doc1 = formatTaggedSentences (either (T.pack . show . _emuid . _uid) (T.singleton . (^._1))) sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text. either formatLinkedMention formatIndexedTimex) $ map (\(TagPos (_,_,x)) -> x) clst
  in hsep 10 left [doc1,doc2]




formatDocStructure :: Bool -> DocStructure -> [Text]
formatDocStructure showdetail (DocStructure mtokenss sentitems mergedtags sstrs) =
  let line1 = [ "=================================================================================================="
              , "-- Time and NER tagged text ----------------------------------------------------------------------"
              , T.pack (render (formatTagged mtokenss sentitems mergedtags))
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]
      line3 = concatMap (maybe [""] (formatSentStructure showdetail)) sstrs

  in line1  ++ line3


formatSentStructure :: Bool -> SentStructure -> [Text]
formatSentStructure _showdetail ss@(SentStructure i ptr _ _ _ _ vstrs) =
   let subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
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
    T.pack (printf "Verb: %-20s" (vp^.vp_lemma.to unLemma))
  , T.pack (formatSenses False senses mrmmtoppatts)
  ]


-- maybe "" show (dp^?complement._Just.headX)

showMatchedFE :: PreAnalysis '[Lemma] -> (FNFrameElement,MatchedElement) -> String
--                                         FE   range prep text
showMatchedFE tagged (fe,(_,CompVP_DP rng_dp)) = printf "%-15s: %-7s %3s %s" (unFNFrameElement fe) ("DP" ++ show rng_dp)  ("" :: Text) ("" :: Text) -- (headTextDP tagged rng_dp)
showMatchedFE tagged (fe,(_,CompVP_AP rng_ap)) = printf "%-15s: %-7s %3s %s" (unFNFrameElement fe) ("AP" ++ show rng_ap)  ("" :: Text) ("" :: Text) -- ((T.intercalate " " . tokensByRange tagged) (ap^.maximalProjection))
showMatchedFE tagged (fe,(_,CompVP_CP rng_cp)) = printf "%-15s: %-7s %3s %s" (unFNFrameElement fe) ("CP" ++ show rng_cp) ("" :: Text) ("" :: Text) -- ((T.intercalate " " . tokensByRange tagged) rng_cp)
--  where rng_cp = cp^.maximalProjection
showMatchedFE tagged (fe,(_,CompVP_PP rng_pp)) = printf "%-15s: %-7s %3s %s" (unFNFrameElement fe) ("PP" ++ show rng_pp) ("" :: Text) ("" :: Text) -- ((T.intercalate " " . tokensByRange tagged) rng_cp)

{-
  let prep = case pp^.headX.hp_prep of
               Prep_NULL -> ""
               Prep_WORD p -> p
      pclass :: Text
      pclass = case pp^.headX.hp_pclass of
                 PC_Time -> "time"
                 PC_Other -> ""
  in case pp^.complement of
       CompPP_DP dp    -> printf "%-15s: %-7s %3s(%4s) %s" (unFNFrameElement fe) (maybe "" show (dp^?complement._Just.headX)) prep pclass (headTextDP tagged dp)
       CompPP_Gerund rng -> printf "%-15s: %-7s %3s(%4s) %s" (unFNFrameElement fe) (show rng) prep pclass (T.intercalate " " (tokensByRange tagged rng))
-- showMatchedFE tagged (fe,CompVP_Unresolved rng) = printf "%-15s: %-7s %3s %s" (unFNFrameElement fe) (show rng) ("UNKNOWN" :: Text) (T.intercalate " " (tokensByRange tagged rng))
-}



showMatchedFrame :: FrameDB
                 -> PreAnalysis '[Lemma]
                 -> (X'Tree 'PH1,VerbStructure, CP 'PH1)
                 -> IO ()
showMatchedFrame framedb tagged (x'tr,vstr,cp) = do
  T.IO.putStrLn "---------------------------"
  flip traverse_ (matchFrame framedb x'tr (vstr,cp)) $ \(rng,_,x'tr,FMR idiom frame mselected _,_) -> do
    putStrLn ("predicate: "  <> show rng)
    T.IO.putStrLn ("Verb: "  <> T.intercalate " " idiom)
    T.IO.putStrLn ("Frame: " <> unFNFrame frame)
    flip traverse_ mselected $ \(_,felst) -> mapM_ (putStrLn . showMatchedFE tagged) felst



formatMGEdge :: MGEdge -> Text
formatMGEdge e = format "i{} -> i{} [label=\"{}\" style=\"{}\" fontsize=12.0 {}];"
                   (e^.me_start
                   ,e^.me_end
                   ,unFNFrameElement (e^.me_relation) <> maybe "" (":" <>) (e^.me_prep)
                   ,if e^.me_relation == "ref" then "dotted" else if (e^.me_ismodifier) then "bold" else "solid" :: Text
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
formatMGVertex (MGPredicate i _ f (PredVerb idiom _ v))
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
formatMGVertex (MGPredicate i _ f (PredNominalized n _))
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
formatMGVertex (MGEntity i _ _ _ t ns)
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
