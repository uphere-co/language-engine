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
import           Lexicon.Format                          (formatArgPattStat,formatRoleMap)
import           Lexicon.Type                            (ArgPattern(..),RoleInstance,GRel(..))
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb                    (vp_aspect,vp_auxiliary,vp_lemma,vp_negation,vp_tense)
import           NLP.Syntax.Type.XBar                    (CPDP)
import           NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos                         (CharIdx,TokIdx,TagPos(..),SentItem)
import qualified WikiEL                        as WEL
import           WikiEL.EntityLinking                    (UIDCite(..),_emuid)
import qualified WikiEL.EntityLinking          as EL
import           WikiEL.WikiNamedEntityTagger            (PreNE,resolvedUID)
--
import           SRL.Analyze.Match                       (matchFrame)
import           SRL.Analyze.Type                        (ExceptionalFrame(..),ONSenseFrameNetInstance(..)
                                                         ,DocStructure(..),SentStructure(..),VerbStructure(..)
                                                         ,MGVertex(..),MeaningGraph
                                                         ,mg_vertices,mg_edges
                                                         ,me_relation,me_prep,me_start,me_end
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



formatFrame :: Maybe (ONSenseFrameNetInstance, Int) -> String
formatFrame t =
  printf " %-20s | %-40s      ------      %-30s\n"
    (fromMaybe "" (t ^? _Just . _1 . onfn_frame . to (either formatExFrame (^.tf_frameID))))
    (maybe "" (T.intercalate ", ") (t^?_Just._1.onfn_frame._Right.tf_feCore))
    (maybe "" (T.intercalate ", ") (t^?_Just._1.onfn_frame._Right.tf_fePeri))


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [(ONSenseFrameNetInstance,Int)]
             -> [((RoleInstance,Int), [(ArgPattern () GRel,Int)])]
             -> String
formatSenses doesShowOtherSense onfnlst rmtoppatts
  = let t = chooseMostFreqFrame onfnlst
    in "Top frame: "
       ++ (if (not.null) t then formatFrame (Just (head t)) else formatFrame Nothing)
       ++ "--------------------------------------------------------------------------------------------------\n"
       ++ intercalate "\n" (flip map rmtoppatts (\((rm,_),toppatts) ->
               let argpattstr = formatArgPattStat toppatts
               in (formatRoleMap (rm^._2) ++ ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<> argpattstr))))
       ++ "\n--------------------------------------------------------------------------------------------------\n"
       ++ if doesShowOtherSense
          then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense onfnlst)
          else ""


formatLinkedMention :: EL.EntityMention Text -> String
formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (EL._emuid _uid) (EL._emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (EL._emuid _uid)                  (formatEMInfo _info)


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


formatEMInfo :: EL.EMInfo Text -> String
formatEMInfo em@(_,_,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)


formatTagged :: [[Maybe Token]]
             -> [SentItem CharIdx]
             -> [TagPos TokIdx (Either (EL.EntityMention Text) (Char,(Maybe Text)))]
             -> Box
formatTagged mtokenss sentitems tlst =
  let toks = concatMap (map snd . sentToTokens') mtokenss
      clst = mapMaybe (convertTagPosFromTokenToChar toks) tlst
      sents_tagged = map (addTag clst) sentitems
      doc1 = formatTaggedSentences (either (T.pack . show . _emuid . EL._uid) (T.singleton . (^._1))) sents_tagged
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
formatSentStructure showdetail (SentStructure i ptr _ clausetr mcpstr vstrs) =
   let subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
                  , formatIndexTokensFromTree 0 ptr
                  ]
       subline1_1 = [ "--------------------------------------------------------------------------------------------------"
                    , formatClauseStructure clausetr
                    , "================================================================================================="
                    ]
       subline2 = map (formatVerbStructure clausetr mcpstr) vstrs
   in subline1 ++ (if showdetail then subline1_1 else []) ++ concat subline2


formatVerbStructure :: ClauseTree -> Maybe [Bitree (Range,CPDP '[Lemma]) (Range,CPDP '[Lemma])] -> VerbStructure -> [Text]
formatVerbStructure clausetr mcpstr (VerbStructure vp senses mrmmtoppatts) =
  [ formatVPwithPAWS [] clausetr mcpstr vp        -- for the time being
  , T.pack (printf "Verb: %-20s" (vp^.vp_lemma.to unLemma))
  , T.pack $ (formatSenses False senses mrmmtoppatts)
  ]




showMatchedFrame :: (Maybe [Bitree (Range, CPDP '[Lemma]) (Range, CPDP '[Lemma])]
                    ,VerbStructure
                    ,PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
                 -> IO ()
showMatchedFrame (_,vstr,paws) = do
  let gettokens = T.intercalate " " . map (tokenWord.snd) . toList . current
  T.IO.putStrLn "---------------------------"
  flip traverse_ (matchFrame (vstr,paws)) $ \(rng,_,frame,mselected) -> do
    putStrLn ("predicate: " <> show rng)
    T.IO.putStrLn ("Verb: " <> (vstr^.vs_vp.vp_lemma.to unLemma))
    T.IO.putStrLn ("Frame: " <> frame)
    flip traverse_ mselected $ \(_,felst) -> do
      mapM_ putStrLn . map (\(fe,(mp,z)) -> printf "%-15s: %-7s %3s %s"
                                                   fe
                                                   (show (getRange (current z)))
                                                   (fromMaybe "" mp)
                                                   (gettokens z))
        $ felst





dotMeaningGraph :: String -> MeaningGraph -> String
dotMeaningGraph title mg = printf "digraph G {\n  %s\n  %s\n  %s\n}" vtxt etxt ttxt
  where
    fmtEdge e = printf "i%d -> i%d [label=\"%s\" fontsize=12.0];"
                  (e^.me_start) (e^.me_end) (e^.me_relation <> maybe "" (":" <>) (e^.me_prep))
    fmtVerb (MGEntity    _ _ _  ) = Nothing
    fmtVerb (MGPredicate i _ f v) = Just (i,f <> " | { "
                                              <> T.intercalate " " (v^..vp_auxiliary.traverse._1) <> " | "
                                              <> fromMaybe "" (v^?vp_negation._Just._1) <> " | "
                                              <> v^.vp_lemma.to unLemma <> " | "
                                              <> formatTense (v^.vp_tense) <> "." <> formatAspect (v^.vp_aspect)
                                              <> " } " )
    
    vtxt :: String
    vtxt =
      let vertices = mg^.mg_vertices
          verbs = mapMaybe fmtVerb vertices
          entities = mapMaybe (\case MGEntity i _ t -> Just (i,T.replace "\"" "\\\"" t); MGPredicate _ _ _ _ -> Nothing) vertices
      in (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=record style=filled, fillcolor=grey label=\"{%s}\"];" i t)) verbs ++  "\n  " ++
         (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=record label=\"{ %s }\"];" i t)) entities
    etxt :: String
    etxt =      
      let edges = mg^.mg_edges
      in (intercalate "\n " . map fmtEdge) edges
    ttxt :: String
    ttxt = "labelloc=\"t\"; \n " ++ "label=\"" ++ title ++ "\"; \n " 
