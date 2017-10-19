{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module SRL.Analyze.Match where

import           Control.Applicative
import           Control.Error.Safe           (rightMay,headErr)
import           Control.Lens
import           Control.Lens.Extras          (is)
import           Control.Monad                (guard,join)
import           Data.Foldable
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                  ((<>))
import qualified Data.Text              as T
import           Data.Text                    (Text)
--
import           Data.Bitree                  (getNodes,getRoot,getRoot1,_PN)
import           Data.BitreeZipper            (current,mkBitreeZipper,root)
import           Data.Range                   (Range,elemRevIsInsideR,isInsideR)
import           Lexicon.Mapping.Causation    (causeDualMap,cm_baseFrame,cm_causativeFrame
                                              ,cm_externalAgent,cm_extraMapping)
import           Lexicon.Type
import           NLP.Syntax.Clause            (cpRange,findPAWS)
import           NLP.Syntax.Format            (formatCP,formatDP)
import           NLP.Syntax.Noun              (splitPP)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (isLemmaAs,intLemma0)
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
import           NLP.Type.TagPos              (TagPos,TokIdx)
--
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,DocStructure
                                              ,SentStructure
                                              ,VerbStructure
                                              ,ds_sentStructures
                                              ,ss_clausetr,ss_cpstr,ss_tagged,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp
                                              ,me_relation
                                              ,mv_text,mv_range,mv_id,mv_resolved_entities,mg_vertices,mg_edges
                                              )
--
import Debug.Trace

import Text.Printf
import Data.List (intercalate)


mkPAWSTriples :: SentStructure
              -> ([X'Tree '[Lemma]],[(VerbStructure, PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))])
mkPAWSTriples sstr =
  let clausetr = sstr^.ss_clausetr
      cpstr = sstr^.ss_cpstr
  in ( cpstr
     , [(vstr,paws)| vstr <- sstr ^.ss_verbStructures
                   , let vp = vstr^.vs_vp
                   , paws <- maybeToList (findPAWS (sstr^..ss_tagged.traverse.to (fmap snd)) clausetr vp cpstr) ]
     )


pbArgForGArg :: GArg -> ArgPattern p GRel -> Maybe (Text,GRel)
pbArgForGArg garg patt = check patt_arg0 "arg0" <|>
                         check patt_arg1 "arg1" <|>
                         check patt_arg2 "arg2" <|>
                         check patt_arg3 "arg3" <|>
                         check patt_arg4 "arg4"
  where check l label = do a <- patt^.l
                           garg' <- findGArg a
                           if garg==garg' then Just (label,a) else Nothing


pbArgForPP :: ArgPattern p GRel -> [(Text,(Text,Maybe Bool))]
pbArgForPP patt = catMaybes [ check patt_arg0 "arg0"
                            , check patt_arg1 "arg1"
                            , check patt_arg2 "arg2"
                            , check patt_arg3 "arg3"
                            , check patt_arg4 "arg4"
                            ]
  where check l label = do a <- patt^.l
                           case a of
                             GR_PP (Just (prep,ising)) -> return (label,(prep,Just ising))
                             _           -> Nothing


matchSubject :: [(PBArg,FNFrameElement)]
             -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, CompVP '[Lemma])
matchSubject rolemap mDP patt = do
  dp <- mDP^?_Just._Right            -- for the time being
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,CompVP_DP dp) <$> lookup p rolemap


isPhiOrThat cp = case cp^.headX of
                   C_PHI -> True
                   C_WORD z -> isLemmaAs "that" (current z)



matchObjects :: [(PBArg,FNFrameElement)]
             -> VerbP '[Lemma]
             -> ArgPattern p GRel
             -> [(FNFrameElement, CompVP '[Lemma])]
matchObjects rolemap verbp patt = do
  (garg,obj) <- zip [GA1,GA2] (verbp^..complement.traverse.trResolved._Just)
  guard (case obj of CompVP_CP _ -> True; CompVP_DP _ -> True; _ -> False)
  (p,a) <- maybeToList (pbArgForGArg garg patt)
  case obj of
    CompVP_CP cp -> guard (isPhiOrThat cp && a == GR_SBAR (Just garg))
    CompVP_DP _  -> guard (a == GR_NP   (Just garg))
    _            -> []
  fe <- maybeToList (lookup p rolemap)
  return (fe, obj)



matchPP :: [TagPos TokIdx MarkType]
        -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
        -> (Maybe Text,Maybe PrepClass,Maybe Bool)
        -> Maybe (Text,Zipper '[Lemma],DetP '[Lemma])
matchPP tagged paws (mprep,mpclass,mising) = do
    Left (rng,S_PP prep' _ _) <- find ppcheck (paws^.pa_candidate_args)
    let tr = paws^.pa_CP.maximalProjection.to root.to current
    z' <- (find (\z -> z^?to current._PN._1._1 == Just rng) . getNodes .mkBitreeZipper []) tr
    return (prep',z',splitPP tagged z')
  where
    ppcheck (Left (_,S_PP prep' pclass' ising')) = maybe True (== prep')   mprep   &&
                                                   maybe True (== pclass') mpclass &&
                                                   maybe True (\ising -> ising == ising') mising
    ppcheck _                            = False


matchPrepArgs :: [(PBArg,FNFrameElement)]
              -> [TagPos TokIdx MarkType]
              -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
              -> ArgPattern p GRel
              -> [(FNFrameElement, CompVP '[Lemma])]
              -> [(FNFrameElement, CompVP '[Lemma])]
matchPrepArgs rolemap tagged paws patt felst = do
  (p,(prep,mising)) <- pbArgForPP patt
  role <- maybeToList (lookup p rolemap)

  (_,o,z) <- maybeToList (matchPP tagged paws (Just prep,Nothing,mising))
  let comp = CompVP_PP (XP (Prep_WORD prep,PC_Other) o () () z)
      rng = compVPToRange comp
  guard (is _Nothing (find (\x -> x^?_2.to compVPToRange == Just rng) felst))
  return (role, comp)


matchAgentForPassive :: [(PBArg,FNFrameElement)]
                     -> [TagPos TokIdx MarkType]
                     -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, CompVP '[Lemma])
matchAgentForPassive rolemap tagged paws patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    (_,o,z) <- matchPP tagged paws (Just "by",Just PC_Other,Nothing)
    let comp = CompVP_PP (XP (Prep_WORD "by",PC_Other) o () () z)
    (,comp) <$> lookup p rolemap


matchSO :: [(PBArg,FNFrameElement)]
        -> [TagPos TokIdx MarkType]
        -> ( Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
           , VerbP '[Lemma]
           , PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, CompVP '[Lemma])])
matchSO rolemap tagged (mDP,verbp,paws) (patt,num) =
  let (rpatt,rmatched0) = case verbp^.headX.vp_voice of
                            Active  -> ((patt,num), maybeToList (matchSubject rolemap mDP patt) ++ matchObjects rolemap verbp patt)
                            Passive -> ((patt,num),maybeToList (matchAgentForPassive rolemap tagged paws patt) ++ matchObjects rolemap verbp patt) 
      rmatched1 = rmatched0 ++ maybeToList (matchExtraRolesForPPTime tagged paws rmatched0)
  in (rpatt,rmatched1 ++ matchPrepArgs rolemap tagged paws patt rmatched1)

-- ++ matchPrepArgs rolemap tagged paws patt)

extendRoleMapForDual :: (Text,SenseID,[(PBArg,FNFrameElement)])
                     -> (Text,SenseID,[(PBArg,FNFrameElement)])
extendRoleMapForDual (frame,sense,rolemap) = fromMaybe (frame,sense,rolemap) $ do
  dualmap <- lookup frame $ map (\c -> (c^.cm_baseFrame,c)) causeDualMap
  let frame' = dualmap^.cm_causativeFrame
      rolemap' = filter (\(k,_v) -> k /= "frame" && k /= "arg0") rolemap
      rolemap'' =  map f rolemap'
        where f (k,v) = maybe (k,v) (k,) (lookup v (dualmap^.cm_extraMapping))
      rolemap''' = ("arg0",dualmap^.cm_externalAgent) : rolemap''
  return (frame',sense,rolemap''')


numMatchedRoles :: ((ArgPattern () GRel, Int), [(FNFrameElement, a)]) -> Int
numMatchedRoles = lengthOf (_2.folded)


matchRoles :: [(PBArg,FNFrameElement)]
           -> [TagPos TokIdx MarkType]
           -> VerbP '[Lemma]
           -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
           -> [(ArgPattern () GRel, Int)]
           -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
           -> Maybe ((ArgPattern () GRel, Int),[(FNFrameElement, CompVP '[Lemma])])
matchRoles rolemap tagged verbp paws toppattstats mDP =
    (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy (flip compare `on` numMatchedRoles)) matched
  where
    matched = map (matchSO rolemap tagged (mDP,verbp,paws)) toppattstats
    cmpstat  = flip compare `on` (^._1._2)
    eq       = (==) `on` lengthOf (_2.folded)



matchFrameRolesForCauseDual :: [TagPos TokIdx MarkType]
                            -> VerbP '[Lemma]
                            -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
                            -> [(ArgPattern () GRel,Int)]
                            -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
                            -> LittleV
                            -> (Text, SenseID, [(PBArg, FNFrameElement)])
                            -> (Text, (SenseID,Bool) , Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])]))
matchFrameRolesForCauseDual tagged verbp paws toppatts mDP causetype (frame1,sense1,rolemap1) =
  let (frame2,sense2,rolemap2) = if causetype == LVDual
                                 then extendRoleMapForDual (frame1,sense1,rolemap1)
                                 else (frame1,sense1,rolemap1)
      mselected1 = matchRoles rolemap1 tagged verbp paws toppatts mDP
      mselected2 = matchRoles rolemap2 tagged verbp paws toppatts mDP
  in case (mselected1,mselected2) of
       (Nothing,Nothing) -> (frame1,(sense1,False),Nothing)
       (Just _ ,Nothing) -> (frame1,(sense1,False),mselected1)
       (Nothing,Just _ ) -> (frame2,(sense2,True),mselected2)
       (Just s1,Just s2) ->
         case (compare `on` numMatchedRoles) s1 s2 of
           GT -> (frame1,(sense1,False),mselected1)
           LT -> (frame2,(sense2,True),mselected2)
           EQ -> (frame1,(sense1,False),mselected1)   -- choose intransitive because transitive should
                                              -- have one more argument in general.


matchFrameRolesAll :: [TagPos TokIdx MarkType]
                   -> VerbP '[Lemma]
                   -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
                   -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
                   -> [((RoleInstance,Int),[(ArgPattern () GRel,Int)])]
                   -> [((Text,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])])),Int)]
matchFrameRolesAll tagged verbp paws mDP rmtoppatts = do
  (rm,toppatts) <- rmtoppatts
  let sense1 = rm^._1._1
      rolemap1 = rm^._1._2
      stat = rm^._2
  frame1 <- maybeToList (lookup "frame" rolemap1)
  causetype <- (\x -> if x == "dual" then LVDual else LVSingle) <$> maybeToList (lookup "cause" rolemap1)
  return (matchFrameRolesForCauseDual tagged verbp paws toppatts mDP causetype (frame1,sense1,rolemap1),stat)


matchExtraRolesForPPTime tagged paws felst = do
  guard (isNothing (find (\x -> x^._1 == "Time") felst))
  (prep,o,z) <-matchPP tagged paws (Nothing,Just PC_Time,Just False)
  let rng = headRange z
      comp = CompVP_PP (XP (Prep_WORD prep,PC_Time) o () () z)
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to headRange == Just rng) felst))
  return ("Time",comp)

  
matchExtraRolesForPPing prep role tagged paws felst = do
  guard (isNothing (find (\x -> x^._1 == role) felst))
  (_,o,z) <-matchPP tagged paws (Just prep,Just PC_Other,Just True)
  let rng = headRange z
      comp = CompVP_PP (XP (Prep_WORD prep,PC_Other) o () () z)
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to headRange == Just rng) felst))
  return (role,comp)


matchExtraRolesForCPInCompVP check role paws felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let candidates = paws^..pa_CP.complement.complement.complement.traverse.trResolved._Just._CompVP_CP
  cp <- find check candidates
  let rng = cp^.maximalProjection.to current.to getRange
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to headRange == Just rng) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.to cpRange == Just rng) felst))
  let comp = CompVP_CP cp
  return (role,comp)


matchExtraRolesForCPInAdjunctCP mcheck role paws felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let candidates = paws^..pa_CP.adjunct.traverse._AdjunctCP_CP
  cp <- case mcheck of
          Nothing -> (rightMay . headErr "no adjuncts") candidates
          Just check -> find check candidates
  let rng = cp^.maximalProjection.to current.to getRange
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to headRange == Just rng) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.to cpRange == Just rng) felst))
  let comp = CompVP_CP cp
  return (role,comp)


hasComplementizer lst x =
  x^?headX._C_WORD.to (\z -> any (\c -> isLemmaAs c (current z)) lst) == Just True


toInfinitive x =
  let vprop = x^.complement.complement.headX
  in case vprop^.vp_auxiliary of
       (_,(_,lma)):_ -> lma == "to"
       _             -> False


-- | this function should be generalized.
--
matchExtraRoles :: [TagPos TokIdx MarkType]
                -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
                -> [(FNFrameElement, CompVP '[Lemma])]
                -> [(FNFrameElement, CompVP '[Lemma])]
matchExtraRoles tagged paws felst =
  let mmeans = matchExtraRolesForPPing "by" "Means" tagged paws felst
      felst' = felst ++ maybeToList mmeans
      mcomp  = matchExtraRolesForCPInCompVP (hasComplementizer ["after","before"]) "Relative_time"    paws felst' <|>    -- for the time being
               matchExtraRolesForCPInCompVP toInfinitive                           "Purpose" paws felst' <|>
               matchExtraRolesForPPing "after" "Relative_time" tagged paws felst'                                 <|>
               matchExtraRolesForPPing "before" "Relative_time" tagged paws felst'
      felst'' = felst' ++ maybeToList mcomp
      madj   = matchExtraRolesForCPInAdjunctCP (Just (hasComplementizer ["after","before"])) "Relative_time"    paws felst'' <|>
               matchExtraRolesForCPInAdjunctCP (Just (hasComplementizer ["while","as"]))     "Manner"  paws felst'' <|>
               matchExtraRolesForCPInAdjunctCP (Just toInfinitive)                           "Purpose" paws felst'' <|>
               matchExtraRolesForCPInAdjunctCP Nothing                                       "Manner"  paws felst''
  in felst'' ++ maybeToList madj


-- | A scoring algorithm for selecting a frame among candidates.
--   This version is ad hoc, so it will be updated when we come up with a better algorithm.
--
scoreSelectedFrame :: Int
                   -> ((Text,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement,a)])),Int)
                   -> Double
scoreSelectedFrame total ((_,_,mselected),n) =
  let mn = maybe 0 fromIntegral (mselected^?_Just.to numMatchedRoles)
  in mn * (fromIntegral n) / (fromIntegral total) * roleMatchWeightFactor + (mn*(fromIntegral total))



-- | Resolve PP ambiguity for matched PP
--   This algorithm is ad hoc but practically works for PP embedded in DP by CoreNLP.
--   We need to have a systematic treatment and tests for PP ambiguity.
--
resolveAmbiguityInDP :: [(FNFrameElement, CompVP '[Lemma])]
                     -> [(FNFrameElement, CompVP '[Lemma])]
resolveAmbiguityInDP [] = []
resolveAmbiguityInDP lst = foldr1 (.) (map go lst) lst
  where
    go :: (FNFrameElement,CompVP '[Lemma])
       -> [(FNFrameElement,CompVP '[Lemma])]
       -> [(FNFrameElement,CompVP '[Lemma])]
    go (fe,CompVP_PP pp) lst = let o = pp^.maximalProjection
                               in map (f (fe,getRange (current o))) lst
    go (fe,CompVP_DP dp) lst = map (f (fe,dp^.headX._2)) lst
    go (fe,_           ) lst = lst

    f (fe,rng@(b,e)) (fe',CompVP_PP pp)
      = let prep' = pp^.headX
            o'    = pp^.maximalProjection
            z'    = pp^.complement
            rng'@(b',e') = z'^.headX._1
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let z'' = ((headX .~ ((b,e),(b',b-1))) . (adjunct .~ Nothing)) z'
               in (fe',CompVP_PP (XP prep' o' () () z''))
          else (fe',CompVP_PP (XP prep' o' () () z'))
    f (fe,rng@(b,e)) (fe',CompVP_DP dp)
      = let rng'@(b',e') = dp^.headX._1
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let dp' = ((headX .~ ((b,e),(b',b-1))) . (adjunct .~ Nothing)) dp
               in (fe', CompVP_DP dp')
          else (fe', CompVP_DP dp)
    f (fe,rng@(b,e)) x = x



matchFrame :: [TagPos TokIdx MarkType]
           -> (VerbStructure,PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag)))
           -> Maybe (Range,VerbProperty (Zipper '[Lemma])
                    ,Text
                    ,(SenseID,Bool)
                    ,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])]))
matchFrame tagged (vstr,paws) = do
  let cp = paws^.pa_CP
      verbp = cp^.complement.complement
      mDP = cp^.complement.specifier.trResolved
      vprop = vstr^.vs_vp
      rng = cpRange cp
      frmsels = matchFrameRolesAll tagged verbp paws mDP (vstr^.vs_roleTopPatts)
      total=  sum (frmsels^..traverse._2)
  ((frame,sense,mselected0),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)
  let mselected1 = (_Just . _2 %~ matchExtraRoles tagged paws) mselected0
      mselected  = (_Just . _2 %~ resolveAmbiguityInDP) mselected1
  return (rng,vprop,frame,sense,mselected)




depCPDP :: X'Tree p -> [(Range,Range)]
depCPDP (PN (rng0,_) xs) = map ((rng0,) . fst . getRoot1) xs ++ concatMap depCPDP xs
depCPDP (PL _)           = []



entityFromDP :: DetP t -> (Range,Text,Maybe (Range,Text))
entityFromDP dp =
  let rng = headRange dp
      txt = headText dp
      mrngtxt' = do rng_sub <- case (dp^.adjunct, dp^.complement) of
                                 (Just r,_) -> return r -- for the time being
                                 _          -> Nothing
                    let txt_sub = dp ^.  maximalProjection
                                        .to current
                                        .to (tokensByRange rng_sub)
                                        .to (T.intercalate " ")
                    return (rng_sub,txt_sub)
  in (rng,txt,mrngtxt')


showMatchedFE' :: (FNFrameElement, CompVP '[Lemma]) -> String
--                                         FE   range prep text
showMatchedFE' (fe,CompVP_DP dp) = printf "%-15s: %-7s %3s %s" fe (show (headRange dp)) ("" :: Text) (headText dp)
showMatchedFE' (fe,CompVP_CP cp) = printf "%-15s: %-7s %3s %s" fe (z^.to current.to getRange.to show) ("" :: Text) (gettext z)
  where z = cp^.maximalProjection
        gettext = T.intercalate " " . map (tokenWord.snd) . toList . current
showMatchedFE' (fe,CompVP_PP pp) = printf "%-15s: %-7s %3s %s" fe (show (headRange dp)) prep (headText dp)
  where dp = pp^.complement
        prep = case pp^.headX._1 of
                 Prep_NULL -> ""
                 Prep_WORD p -> p
showMatchedFE' (fe,CompVP_Unresolved z) = printf "%-15s: %-7s %3s %s" fe ((show.getRange.current) z) ("UNKNOWN" :: Text) (gettext z)
  where gettext = T.intercalate " " . map (tokenWord.snd) . toList . current



meaningGraph :: SentStructure -> MeaningGraph
meaningGraph sstr =
  let (cpstr,lst_vstrpaws) = mkPAWSTriples sstr
      matched = mapMaybe (matchFrame (sstr^..ss_tagged.traverse.to (fmap snd))) lst_vstrpaws
      depmap = depCPDP =<< cpstr
      --
      preds = flip map matched $ \(rng,vprop,frame,sense,_mselected) i
                                   -> MGPredicate i rng frame sense (simplifyVProp vprop)
      ipreds = zipWith ($) preds [1..]
      --

      entities0 = do (_,_,_,_,mselected) <- matched
                     (_,felst) <- maybeToList mselected
                     (fe,x) <- felst
                     -- trace (intercalate "\n" (map showMatchedFE' felst)) $ do
                     case x of
                       CompVP_Unresolved _ -> []
                       CompVP_CP cp -> [] -- CP is not an entity.
                       CompVP_DP dp -> return (entityFromDP dp)
                       CompVP_PP pp -> return (entityFromDP (pp^.complement))

      filterFrame = filter (\(rng,_,_) -> not (any (\p -> p^.mv_range == rng) ipreds))
      --

      entities1 = filterFrame
                . map head
                . groupBy ((==) `on` (^._1))
                . sortBy (compare `on` (^._1))
                $ entities0

      mkEntityFun (rng,txt,mrngtxt') =
        (\i -> MGEntity i rng txt []) :
          flip (maybe []) mrngtxt' (\(rng',txt') -> [ \i'  -> MGEntity i' rng' txt' []
                                                    , \i'' -> MGNominalPredicate i'' rng' "Instance"
                                                    ]
                                   )


      entities = concatMap mkEntityFun entities1

      vertices = ipreds ++ zipWith ($) entities (enumFrom (length ipreds+1))
      --
      rangeid :: MGVertex -> (Int,Range)
      rangeid (MGEntity _ rng _ _)         = (0,rng)
      rangeid (MGPredicate _ rng _ _ _)    = (0,rng)
      rangeid (MGNominalPredicate _ rng _) = (1,rng)
      rngidxmap = HM.fromList [(rangeid v, v^.mv_id) | v <- vertices ]
      edges0 = do (rng,_,_,_,mselected) <- matched
                  i <- maybeToList (HM.lookup (0,rng) rngidxmap)   -- frame
                  (_,felst) <- maybeToList mselected
                  (fe,x) <- felst
                  (rng',mprep) <- case x of
                                    CompVP_Unresolved _ -> []
                                    CompVP_CP cp -> let z_cp = cp^.maximalProjection
                                                        mprep = case cp^.headX of
                                                                  C_PHI -> Nothing
                                                                  C_WORD z -> (z^?to current.to intLemma0._Just._2.to unLemma)
                                                                                 >>= \prep -> if prep == "that" then Nothing else return prep
                                                    in return (getRange (current z_cp),mprep)
                                    CompVP_DP dp -> return (headRange dp,Nothing)
                                    CompVP_PP pp -> return (headRange (pp^.complement),pp^?headX._1._Prep_WORD)
                  i' <- maybeToList (HM.lookup (0,rng') rngidxmap)  -- frame element
                  let b = isJust (find (== (rng',rng)) depmap)
                  return (MGEdge fe b mprep i i')
      edges1 = do (rng,_,mrngtxt') <- entities1
                  (rng',_) <- maybeToList mrngtxt'
                  i_frame <- maybeToList (HM.lookup (1,rng') rngidxmap)
                  i_instance <- maybeToList (HM.lookup (0,rng) rngidxmap)
                  i_type     <- maybeToList (HM.lookup (0,rng') rngidxmap)
                  [MGEdge "Instance" True Nothing i_frame i_instance, MGEdge "Type" False Nothing i_frame i_type]

  in MeaningGraph vertices (edges0 ++ edges1)


isEntity :: MGVertex -> Bool
isEntity x = case x of
               MGEntity {..} -> True
               _             -> False


tagMG :: MeaningGraph -> [(Range,Text)] -> MeaningGraph
tagMG mg wikilst =
  let mg' = mg ^.. mg_vertices
                 . traverse
                 . to (\x -> if (x ^. mv_range) `elemRevIsInsideR` (map fst wikilst) && isEntity x
                             then x & (mv_resolved_entities .~ map (^. _2) (filter (\w -> (w ^. _1) `isInsideR` (x ^. mv_range)) wikilst))
                             else x )
  in MeaningGraph mg' (mg ^. mg_edges)

changeMGText mg =
  let mg' = mg ^.. mg_edges
                 . traverse
                 . to (\x -> x & (me_relation .~ (T.replace "&" "-AND-" (x ^. me_relation))))
      mg'' = mg ^.. mg_vertices
                  . traverse
                  . to (\x -> case x of
                           MGEntity {..} -> x & (mv_text .~ (T.replace "&" "-AND-" (x ^. mv_text)))
                           MGPredicate {..} -> x
                           MGNominalPredicate {..} -> x
                       )
  in MeaningGraph mg'' mg'
