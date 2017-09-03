{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module SRL.Analyze.Format where

import           Control.Lens                            ((^.),(^?),_1,_2,_3,_Just,_Right,to)
import           Data.Foldable
import           Data.List                               (find,intercalate,intersperse)
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
import           Lexicon.Merge                           (constructTopPatterns)
import           Lexicon.Query                           (cutHistogram)
import           Lexicon.Type                            (ArgPattern(..),RoleInstance
                                                         ,RolePattInstance,GRel(..))
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.XBar                    (CP)
import           NLP.Type.CoreNLP                        (Token,token_lemma,token_pos)
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                 (Voice)
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
                                                         ,me_relation,me_start,me_end
                                                         ,chooseFrame
                                                         ,onfn_senseID,onfn_definition,onfn_frame
                                                         ,tf_frameID,tf_feCore,tf_fePeri)
import           SRL.Analyze.Util                        (CharIdx,TokIdx,TagPos(..),SentItem
                                                         ,addTag,convertTagPosFromTokenToChar
                                                         ,underlineText)



getTopPatternsFromONFNInst :: [RoleInstance]
                           -> [RolePattInstance Voice]
                           -> ONSenseFrameNetInstance
                           -> Maybe (RoleInstance, Maybe [(ArgPattern () GRel,Int)])
getTopPatternsFromONFNInst rolemap subcats inst = do
  let sid = inst^.onfn_senseID
  rm <- find (\rm -> rm^._1 == sid) rolemap
  let msubcat = find ((== sid) . (^._1)) subcats
      mtoppatts_cut = cutHistogram 0.9 . constructTopPatterns . (^._2) <$> msubcat
  return (rm,mtoppatts_cut)


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
             -> Maybe (RoleInstance, Maybe [(ArgPattern () GRel,Int)])
             -> String
formatSenses doesShowOtherSense onfnlst mrmmtoppatts
  = let t = chooseFrame onfnlst
    in "Top frame: "
       ++ formatFrame t
       ++ "--------------------------------------------------------------------------------------------------\n"
       ++ flip (maybe "") mrmmtoppatts
            (\(rm,mtoppatts) ->
               let margpattstr = fmap formatArgPattStat mtoppatts
               in (formatRoleMap (rm^._2) ++ maybe "" ("\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"<>) margpattstr))
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
formatSentStructure showdetail (SentStructure i ptr vps clausetr mcpstr vstrs) =
   let subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
                  , formatIndexTokensFromTree 0 ptr
                  ]
       subline1_1 = [ "--------------------------------------------------------------------------------------------------"
                    , formatClauseStructure clausetr
                    , "================================================================================================="
                    ]
       subline2 = map (formatVerbStructure clausetr mcpstr) vstrs
   in subline1 ++ (if showdetail then subline1_1 else []) ++ concat subline2


formatVerbStructure :: ClauseTree -> Maybe [Bitree (Range,CP '[Lemma]) (Range,CP '[Lemma])] -> VerbStructure -> [Text]
formatVerbStructure clausetr mcpstr (VerbStructure vp lma senses mrmmtoppatts) =
  [ formatVPwithPAWS clausetr mcpstr vp
  , T.pack (printf "Verb: %-20s" lma)
  , T.pack $ (formatSenses False senses mrmmtoppatts)
  ]




showMatchedFrame :: (Maybe [Bitree (Range, CP '[Lemma]) (Range, CP '[Lemma])]
                    ,VerbStructure
                    ,PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
                 -> IO ()
showMatchedFrame (mcpstr,vstr,paws) = do
  let gettokens = T.intercalate " " . map (tokenWord.snd) . toList . current
  T.IO.putStrLn "---------------------------"
  flip traverse_ (matchFrame (mcpstr,vstr,paws)) $ \(rng,verb,frame,mselected) -> do
    putStrLn ("predicate: " <> show rng) -- maybe "unidentified CP" show (cpRange cp))
    T.IO.putStrLn ("Verb: " <> verb) -- (vstr^.vs_lma))
    T.IO.putStrLn ("Frame: " <> frame)
    flip traverse_ mselected $ \(_,felst) -> do
      mapM_ putStrLn . map (\(fe,z) -> printf "%-15s: %-7s %s" fe (show (getRange (current z))) (gettokens z)) $ felst


dotMeaningGraph :: String -> MeaningGraph -> String
dotMeaningGraph title mg = printf "digraph G {\n  %s\n  %s\n  %s\n}" vtxt etxt ttxt
  where
    vtxt :: String
    vtxt =
      let vertices = mg^.mg_vertices
          verbs = mapMaybe (\case MGEntity _ _ _ -> Nothing ; MGPredicate i _ f v -> Just (i,f <> ":" <> v)) vertices
          entities = mapMaybe (\case MGEntity i _ t -> Just (i,t); MGPredicate _ _ _ _ -> Nothing) vertices
      in (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=box label=\"%s\"];" i t)) verbs ++  "\n  " ++
         (intercalate "\n  " . map (\(i,t) -> printf "i%d [shape=record label=\"{ %s }\"];" i t)) entities
    etxt :: String
    etxt =      
      let edges = mg^.mg_edges
          formatf e = printf "i%d -> i%d [label=\"%s\"];" (e^.me_start) (e^.me_end) (e^.me_relation)
      in (intercalate "\n " . map formatf) edges
    ttxt :: String
    ttxt = "labelloc=\"t\"; \n " ++ "label=\"" ++ title ++ "\"; \n " 
