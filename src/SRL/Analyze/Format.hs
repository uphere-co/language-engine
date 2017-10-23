{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module SRL.Analyze.Format where

import           Control.Lens                            ((^..),(^.),(^?),_1,_2,_3,_Just,_Right,to)
import           Data.Foldable
import           Data.List                               (intercalate,intersperse)
import           Data.Maybe                              (fromMaybe,mapMaybe)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T.IO

import           Text.PrettyPrint.Boxes                  (Box,left,hsep,text,top,vcat,render)
import           Text.Printf                             (printf)

--
import           CoreNLP.Simple.Convert                  (sentToTokens')
import           Data.Bitree
import           Data.BitreeZipper

import           Data.Range
import qualified HTMLEntities.Text             as HTMLT               
import           Lexicon.Format                          (formatArgPattStat,formatRoleMap)
import           Lexicon.Type                            (ArgPattern(..),RoleInstance,GRel(..),FNFrameElement)
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb                    (vp_aspect,vp_auxiliary,vp_lemma,vp_negation,vp_tense)
import           NLP.Syntax.Type.XBar                    (CPDP,CompVP(..),Prep(..),PrepClass(..),X'Tree
                                                         ,headRange,headText,headX,complement,maximalProjection)
import           NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos                         (CharIdx,TokIdx,TagPos(..),SentItem)
import qualified WikiEL                        as WEL
import qualified WikiEL.EntityLinking          as EL
import           WikiEL.Type                             (EMInfo,EntityMention(..),PreNE,UIDCite(..),_emuid)
import           WikiEL.WikiNamedEntityTagger            (resolvedUID)
--
import           SRL.Analyze.Match                       (matchFrame)
import           SRL.Analyze.Type                        (ExceptionalFrame(..),ONSenseFrameNetInstance(..)
                                                         ,DocStructure(..),SentStructure(..),VerbStructure(..)
                                                         ,MGVertex(..),MeaningGraph
                                                         ,mg_vertices,mg_edges
                                                         ,me_relation,me_ismodifier,me_prep,me_start,me_end
                                                         ,chooseMostFreqFrame
                                                         ,onfn_senseID,onfn_definition,onfn_frame
                                                         ,tf_frameID,tf_feCore,tf_fePeri
                                                         ,vs_vp
                                                         )
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




formatSense :: (ONSenseFrameNetInstance,Int) -> String
formatSense (onfninst,num) =
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
             -> [(ONSenseFrameNetInstance,Int)]
             -> [((RoleInstance,Int), [(ArgPattern () GRel,Int)])]
             -> String
formatSenses doesShowOtherSense onfnlst rmtoppatts
  =  "--------------------------------------------------------------------------------------------------\n"
     ++ intercalate "\n--------------------------------------------------------------------------------------------------\n" (flip map rmtoppatts (\((rm,_),toppatts) ->
          let argpattstr = formatArgPattStat toppatts
              mfrm = find (\x -> x^._1.onfn_senseID == rm^._1 ) onfnlst
              framestr = "Frames: " ++ maybe "" formatFrame mfrm
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
formatSentStructure showdetail (SentStructure i ptr _ clausetr cpstr _ vstrs) =
   let subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
                  , formatIndexTokensFromTree 0 ptr
                  ]
       subline1_1 = [ "--------------------------------------------------------------------------------------------------"
                    , formatClauseStructure clausetr
                    , "================================================================================================="
                    ]
       subline2 = map (formatVerbStructure clausetr cpstr) vstrs
   in subline1 ++ (if showdetail then subline1_1 else []) ++ concat subline2


formatVerbStructure :: ClauseTree -> [X'Tree '[Lemma]] -> VerbStructure -> [Text]
formatVerbStructure clausetr cpstr (VerbStructure vp senses mrmmtoppatts) =
  [ formatVPwithPAWS [] clausetr cpstr vp        -- for the time being
  , T.pack (printf "Verb: %-20s" (vp^.vp_lemma.to unLemma))
  , T.pack (formatSenses False senses mrmmtoppatts)
  ]

showMatchedFE :: (FNFrameElement, CompVP '[Lemma]) -> String
--                                         FE   range prep text
showMatchedFE (fe,CompVP_DP dp) = printf "%-15s: %-7s %3s %s" fe (show (headRange dp)) ("" :: Text) (headText dp)
showMatchedFE (fe,CompVP_CP cp) = printf "%-15s: %-7s %3s %s" fe ((show.getRange.current) z) ("" :: Text) (gettext z)
  where z = cp^.maximalProjection
        gettext = T.intercalate " " . map (tokenWord.snd) . toList . current
showMatchedFE (fe,CompVP_PP pp) = printf "%-15s: %-7s %3s(%4s) %s" fe (show (headRange dp)) prep pclass (headText dp)
  where dp = pp^.complement
        prep = case pp^.headX._1 of
                 Prep_NULL -> ""
                 Prep_WORD p -> p
        pclass :: Text
        pclass = case pp^.headX._2 of
                   PC_Time -> "time"
                   PC_Other -> ""
showMatchedFE (fe,CompVP_Unresolved z) = printf "%-15s: %-7s %3s %s" fe ((show.getRange.current) z) ("UNKNOWN" :: Text) (gettext z)
  where gettext = T.intercalate " " . map (tokenWord.snd) . toList . current



showMatchedFrame :: [TagPos TokIdx MarkType]
                 -> (VerbStructure, PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
                 -> IO ()
showMatchedFrame tagged (vstr,paws) = do
  T.IO.putStrLn "---------------------------"
  flip traverse_ (matchFrame tagged (vstr,paws)) $ \(rng,_,frame,_,mselected) -> do
    putStrLn ("predicate: " <> show rng)
    T.IO.putStrLn ("Verb: " <> (vstr^.vs_vp.vp_lemma.to unLemma))
    T.IO.putStrLn ("Frame: " <> frame)
    flip traverse_ mselected $ \(_,felst) -> mapM_ (putStrLn . showMatchedFE) felst




formatMGEdge e = printf "i%d -> i%d [label=\"%s\" style=\"%s\" fontsize=12.0 %s];"
                   (e^.me_start)
                   (e^.me_end)
                   (e^.me_relation <> maybe "" (":" <>) (e^.me_prep))
                   (if (e^.me_ismodifier) then "bold" else "solid" :: Text)
                   (if (e^.me_ismodifier) then "constraint=false" else "" :: Text)
                 ++
                 if (e^.me_ismodifier) then printf "\n  {rankdir=TB; i%d -> i%d [style=invis]};" (e^.me_end) (e^.me_start) else ""

formatMGVerb (MGEntity    _ _ _ _) = Nothing
formatMGVerb (MGPredicate i _ f _ v)
  = Just (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
             "<tr><td colspan=\"4\">" <> f <> "</td></tr>" <>
             "<tr>" <>
             "<td width=\"20\">" <> T.intercalate " " (v^..vp_auxiliary.traverse._1) <> "</td>" <>
             "<td width=\"20\">" <> fromMaybe "" (v^?vp_negation._Just._1)           <> "</td>" <>
             "<td>" <> v^.vp_lemma.to unLemma                           <> "</td>" <>
             "<td>" <> formatTense (v^.vp_tense) <> "." <> formatAspect (v^.vp_aspect) <> "</td>" <>
             "</tr>" <>
             "</table>" )
formatMGVerb (MGNominalPredicate i _ f)
  = Just (i, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
             "<tr><td colspan=\"4\">" <> f <> "</td></tr>" <>
             "<tr>" <>
             "<td width=\"20\">" <> " </td>" <>
             "<td width=\"20\">" <> " </td>" <>
             "<td> </td>" <>
             "<td> Nom.Mod </td>" <>
             "</tr>" <>
             "</table>" )



formatMGEntity (MGEntity i _ t ns  )      = Just (i,"<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">" <>
                                                    "<tr><td>" <> (HTMLT.text t) <> "</td></tr>" <>
                                                    T.concat (map (\x -> "<tr><td>"<> (HTMLT.text x) <>"</td></tr>") ns) <>
                                               "</table>")
formatMGEntity (MGPredicate _ _ _ _ _)    = Nothing
formatMGEntity (MGNominalPredicate _ _ _) = Nothing


dotMeaningGraph :: String -> MeaningGraph -> String
dotMeaningGraph title mg = printf "digraph G {\n  %s\n  %s\n  %s\n}" vtxt etxt ttxt
  where
    -- vtxt :: String
    vtxt = let vertices = mg^.mg_vertices
               verbs = mapMaybe formatMGVerb vertices
               entities = mapMaybe formatMGEntity vertices

           in (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=plaintext, margin=0, style=filled, fillcolor=grey label=<%s>];" i t)) verbs ++  "\n  " ++
              (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=plaintext, margin=0, label=<%s>];" i t)) entities
    --
    -- etxt :: String
    etxt = let edges = mg^.mg_edges in (intercalate "\n  " . map formatMGEdge) edges
    --
    -- ttxt :: String
    ttxt = "labelloc=\"t\"; \n " ++ "label=\"" ++ title ++ "\"; \n "


