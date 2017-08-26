{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module OntoNotes.App.Analyze.Format where

import           Control.Applicative
import           Control.Lens                            ((^..),(^.),(^?),(%~),_1,_2,_3,_4,_5,_6,_7,_Just,_Right,to
                                                         ,lengthOf,folded)
import           Control.Monad                           ((>=>),guard)
import           Data.Foldable
import           Data.Function                           ((&),on)
import           Data.List                               (find,groupBy,intercalate,intersperse,maximumBy,sortBy)
import           Data.Maybe                              (catMaybes,fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T.IO
import           Data.Traversable                        (traverse)
import           Text.PrettyPrint.Boxes                  (Box,left,hsep,text,top,vcat,render)
import           Text.Printf                             (printf)
import           Text.ProtocolBuffers.Basic              (Utf8)
--
import           CoreNLP.Simple.Convert                  (sentToTokens,sentToTokens')
import           CoreNLP.Simple.Type.Simplified          (Token,token_lemma,token_pos)
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util                  (root)
import           Data.Range
import           Lexicon.Format                          (formatArgPatt,formatArgPattStat,formatRoleMap)
import           Lexicon.Merge                           (constructTopPatterns,mergePatterns,patternGraph,patternRelation
                                                         ,listOfSupersetSubset,topPatterns)
import           Lexicon.Query                           (cutHistogram)
import           Lexicon.Type                            (ArgPattern(..),type RoleInstance
                                                         ,type RolePattInstance,POSVorN(..),GRel(..),GArg(..)
                                                         ,patt_arg0,patt_arg1,patt_arg2,patt_arg3,patt_arg4
                                                         ,findGArg
                                                         )
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Printer.PennTreebankII              (formatIndexTokensFromTree)
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                 (Voice)
import qualified WikiEL                        as WEL
import           WikiEL.EntityLinking                    (UIDCite(..),EMInfo,EntityMentionUID,_emuid)
import qualified WikiEL.EntityLinking          as EL
import           WikiEL.WikiNamedEntityTagger            (resolveNEs,getStanfordNEs,parseStanfordNE
                                                         ,namedEntityAnnotator,resolvedUID)

--
import           OntoNotes.App.Util                      (CharIdx,TokIdx,TagPos(..),SentItem
                                                         ,addTag,convertTagPosFromTokenToChar
                                                         ,underlineText)
import           OntoNotes.App.WikiEL                    (linkedMentionToTagPos)
--
import           OntoNotes.App.Analyze.Type              (ExceptionalFrame(..),ONSenseFrameNetInstance(..)
                                                         ,DocStructure(..),SentStructure(..),VerbStructure(..)
                                                         ,chooseFrame
                                                         ,onfn_senseID,onfn_definition,onfn_frame
                                                         ,tf_frameID,tf_feCore,tf_fePeri
                                                         ,vs_lma,vs_vp,vs_mrmmtoppatts
                                                         ,ss_verbStructures, ss_mcpstr, ss_clausetr
                                                         ,ds_sentStructures
                                                         )
--
import Debug.Trace


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



formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (EL._emuid _uid) (EL._emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (EL._emuid _uid)                  (formatEMInfo _info)


formatIndexedTimex (c,mtxt)   = printf "%3s:          %s " (T.singleton c) (fromMaybe "" mtxt)

formatTaggedSentences :: (a -> Text)
                      ->  [(SentItem CharIdx,[TagPos CharIdx a])]
                      -> Box
formatTaggedSentences f sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText f (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map (text . T.unpack) txts


formatPreNE tag = case resolvedUID tag of
                    Left e -> "unresolved"
                    Right i -> show i


formatEMInfo :: EL.EMInfo Text -> String
formatEMInfo em@(_,ws,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)


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
                    , formatClauseStructure vps clausetr
                    , "================================================================================================="
                    ]
       subline2 = map (formatVerbStructure clausetr mcpstr) vstrs
   in subline1 ++ (if showdetail then subline1_1 else []) ++ concat subline2


formatVerbStructure :: ClauseTree -> Maybe [Bitree (Range,CP) (Range,CP)] -> VerbStructure -> [Text]
formatVerbStructure clausetr mcpstr (VerbStructure vp lma senses mrmmtoppatts) =
  [ formatVPwithPAWS clausetr mcpstr vp
  , T.pack (printf "Verb: %-20s" lma)
  , T.pack $ (formatSenses False senses mrmmtoppatts)
  ]



matchFrame mcpstr vstr paws = do
  let cp = paws^.pa_CP
      verbp = cp^.cp_TP.tp_VP
      mrmmtoppatts = vstr^.vs_mrmmtoppatts
      mdp_resolved = resolveDP mcpstr cp
  (rm,mtoppatts) <- mrmmtoppatts
  let rolemap = rm^._2
  frame <- lookup "frame" rolemap
  let selected = do
        toppattstats <- mtoppatts
        dp_resolved <- mdp_resolved
        let matched = map (matchSO rolemap (dp_resolved,verbp,paws)) toppattstats
            cmpmatch = flip compare `on` lengthOf (_2.folded)
            cmpstat  = flip compare `on` (^._1._2)
            eq       = (==) `on` lengthOf (_2.folded)
        (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy cmpmatch) matched
  return (frame,selected)



mkPAWSTriples dstr = do
  msstr <- dstr^.ds_sentStructures
  sstr <- maybeToList msstr
  let clausetr = sstr^.ss_clausetr
      mcpstr = sstr^.ss_mcpstr
  vstr <- sstr ^.ss_verbStructures
  let vp = vstr^.vs_vp
  paws <- maybeToList (findPAWS clausetr vp)
  return (mcpstr,vstr,paws)



showMatchedFrame (mcpstr,vstr,paws) = do
  let cp = paws^.pa_CP
      vp =vstr^.vs_vp
      gettokens = T.intercalate " " . map (tokenWord.snd) . toList . current
  T.IO.putStrLn "---------------------------"
  putStrLn ("predicate: " <> maybe "unidentified CP" show (cpRange cp))
  T.IO.putStrLn ("Verb: " <> (vstr^.vs_lma))
  flip traverse_ (matchFrame mcpstr vstr paws) $ \(frame,mselected) -> do
    T.IO.putStrLn ("Frame: " <> frame)
    flip traverse_ mselected $ \((patt,num),felst) -> do
      mapM_ putStrLn . map (\(fe,z) -> printf "%-15s: %-7s %s" fe (show (getRange (current z))) (gettokens z)) $ felst



matchSO rolemap (dp,verbp,paws) (patt,num) =
  case verbp^.vp_verbProperty.vp_voice of
    Active -> ((patt,num), catMaybes [matchSubject rolemap dp patt, matchObject1 rolemap verbp patt])
    Passive -> ((patt,num),catMaybes [matchAgentForPassive rolemap paws patt,matchThemeForPassive rolemap dp patt])


matchSubject rolemap dp patt = do
  (p,GR_NP (Just GASBJ)) <- subjectPosition patt
  (,dp) <$> lookup p rolemap


matchAgentForPassive rolemap paws patt = do
    (p,GR_NP (Just GASBJ)) <- subjectPosition patt
    Left (rng,_) <- find ppcheck (paws^.pa_candidate_args)
    tr <- current . root <$> paws^.pa_CP.cp_maximal_projection
    pp <- find (\z -> case getRoot (current z) of Left (rng',_) -> rng' == rng; _ -> False) $ getNodes (mkBitreeZipper [] tr)
    (,pp) <$> lookup p rolemap
  where
    ppcheck (Left (_,S_PP "by")) = True
    ppcheck _                    = False

matchThemeForPassive rolemap dp patt = do
  (p,GR_NP (Just GA1)) <- object1Position patt
  (,dp) <$> lookup p rolemap


matchObject1 rolemap verbp patt = do
  obj <- listToMaybe (verbp^.vp_complements)
  Left (_,node) <- Just (getRoot (current obj))
  let ctag = chunkTag node
  (p,a) <- object1Position patt
  case ctag of
    NP   -> guard (a == GR_NP   (Just GA1))
    S    -> guard (a == GR_SBAR (Just GA1))
    SBAR -> guard (a == GR_SBAR (Just GA1))
    _    -> Nothing
  (,obj) <$> lookup p rolemap



matchGRelArg grel patt = check patt_arg0 "arg0" <|>
                         check patt_arg1 "arg1" <|>
                         check patt_arg2 "arg2" <|>
                         check patt_arg3 "arg3" <|>
                         check patt_arg4 "arg4"
  where check l label = patt^.l >>= \a -> findGArg a >>= \grel' -> if grel==grel' then Just (label,a) else Nothing

subjectPosition = matchGRelArg GASBJ

object1Position = matchGRelArg GA1
