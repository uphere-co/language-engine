{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module SRL.Analyze.Match.Frame where

import           Control.Applicative
import           Control.Error.Safe           (rightMay)
import           Control.Lens
import           Control.Lens.Extras          (is)
import           Control.Monad                (guard,join)
import           Data.Function                (on)
import           Data.List                    (find,group,groupBy,sort,sortBy)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe                   (catMaybes,fromMaybe,isNothing,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                  (First(..),(<>))
import           Data.Text                    (Text)
import qualified Data.Text               as T
--
import           Data.BitreeZipper            (current,extractZipperById,toBitree)
import           Data.Range                   (Range,isInsideR)
import           FrameNet.Query.Frame         (FrameDB,frameDB)
import           FrameNet.Type.Frame          (frame_FE,fe_name)
import           Lexicon.Mapping.Causation    (causeDualMap,cm_baseFrame,cm_causativeFrame
                                              ,cm_externalAgent,cm_extraMapping)
import           Lexicon.Type
import           NLP.Syntax.Clause            (constructCP,retrieveResolved)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (GetIntLemma(..),isLemmaAs)
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
import           WordNet.Query                (WordNetDB,lookupLemma,getDerivations)
import           WordNet.Type                 (lex_word)
import           WordNet.Type.POS             (POS(..))
--
import           SRL.Analyze.Match.Preposition (ppRelFrame,ppExtraRoles,ppExtraRoleMap)
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Sense            (getVerbSenses)
import           SRL.Analyze.Type             (SentStructure,VerbStructure,AnalyzePredata(..)
                                              ,analyze_wordnet
                                              ,ss_x'trs,ss_tagged,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp)
import           SRL.Analyze.Type.Match       (EntityInfo(..),FrameMatchResult(..))
--
import Debug.Trace
import NLP.Syntax.Format.Internal




mkTriples :: SentStructure -> [(X'Tree 'PH1, VerbStructure, CP 'PH1)]
mkTriples sstr = do
  vstr <- sstr ^.ss_verbStructures
  let vp = vstr^.vs_vp
      tagged = sstr^.ss_tagged
  cp0 <- maybeToList $ (^._1) <$> constructCP tagged vp -- very inefficient, we should have VP-shell in X'Tree
                                          -- so to find cp directly from X'Tree zipper.
  let rng_cp0 = cp0^.maximalProjection
  w <- maybeToList $ (getFirst . foldMap (First . extractZipperById rng_cp0)) (sstr^.ss_x'trs)
  let x'tr = toBitree w
  cp <- currentCPDPPP w ^.. _CPCase
  return (x'tr,vstr,cp)



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


matchSubject :: X'Tree 'PH1
             -> [(PBArg,Text)]
             -> Maybe (SpecTP 'PH1)
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, CompVP 'PH1)
matchSubject x'tr rolemap mDP patt = do
  dp <- mDP^?_Just._SpecTP_DP            -- for the time being
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,CompVP_DP dp) . FNFrameElement <$> lookup p rolemap


isPhiOrThat :: CP 'PH1 -> Bool
isPhiOrThat cp = case cp^.headX of
                   C_PHI -> True
                   C_WORD word -> word == "that"






matchObjects :: X'Tree 'PH1
             -> [(PBArg,Text)]
             -> VerbP 'PH1
             -> ArgPattern p GRel
             -> [(FNFrameElement, CompVP 'PH1)]
matchObjects x'tr rolemap verbp patt = do
    let resmap = retrieveResolved x'tr
    let compvps = do
          c <- verbp^.complement
          maybeToList (resolvedCompVP resmap c)

    matchNormalObjects compvps <|> matchAdj compvps
  where
    matchNormalObjects compvps = do
      (garg,obj) <- zip [GA1,GA2] (filter (\case CompVP_CP _ -> True; CompVP_DP _ -> True; _ -> False) compvps)
      (p,a) <- maybeToList (pbArgForGArg garg patt)
      case obj of
        CompVP_CP rng_cp -> do
          cp <- maybeToList (extractZipperById rng_cp x'tr >>= \w -> currentCPDPPP w ^? _CPCase)
          guard (isPhiOrThat cp && a == GR_SBAR (Just garg))
        CompVP_DP rng_dp -> do
          dp <- maybeToList (extractZipperById rng_dp x'tr >>= \w -> currentCPDPPP w ^? _DPCase)
          guard (a == GR_NP (Just garg))
        _            -> []
      fe <- FNFrameElement <$> maybeToList (lookup p rolemap)
      return (fe, obj)
    matchAdj compvps = do
      ap <- case compvps of [] -> [] ; (x:_) -> case x of CompVP_AP ap -> [ap] ; _ -> []
      (p,a) <- maybeToList (pbArgForGArg GA1 patt)
      guard (a == GR_NP (Just GA1))
      fe <- FNFrameElement <$> maybeToList (lookup p rolemap)
      return (fe, CompVP_AP ap)


matchPP :: X'Tree 'PH1 -> CP 'PH1 -> (Maybe Text,Maybe PrepClass,Maybe Bool) -> Maybe (PP 'PH1)
matchPP x'tr cp (mprep,mpclass,mising) = do
    let resmap = retrieveResolved x'tr
    let compvps = do
          c <- cp^.complement.complement.complement
          maybeToList (resolvedCompVP resmap c)
        pps_adj = do
          rng_pp <- cp^..complement.complement.adjunct.traverse._AdjunctVP_PP
          maybeToList (extractZipperById rng_pp x'tr >>= \w -> currentCPDPPP w ^? _PPCase)
        pps_comp = do
          compvp <- compvps
          case compvp of
            CompVP_PP rng_pp -> maybeToList (extractZipperById rng_pp x'tr >>= \w -> currentCPDPPP w ^? _PPCase)

              {- let pp's = catMaybes $ do
                    rng <- pp^..complement._CompPP_DP.adjunct.traverse._AdjunctDP_PP
                    return (cpdpppFromX'Tree x'tr rng _PPCase)
              in [pp] -} -- (pp:pp's)  -- now we get back to this
            CompVP_DP _ -> [] {- catMaybes $ do
                              rng <- dp^..adjunct.traverse._AdjunctDP_PP
                              return (cpdpppFromX'Tree x'tr rng _PPCase) -}
            _            -> []
    find ppcheck (pps_comp ++ pps_adj)
  where
    ppcheck pp = let HeadPP prep' pclass' = pp^.headX
                     ising' = is _Just (pp^?complement._CompPP_Gerund)
                 in maybe True (\prep -> Just prep == prep'^?_Prep_WORD) mprep &&
                    maybe True (== pclass') mpclass &&
                    maybe True (== ising') mising


matchPrepArgs :: X'Tree 'PH1
              -> [(PBArg,Text)]
              -> CP 'PH1
              -> ArgPattern p GRel
              -> [(FNFrameElement, CompVP 'PH1)]
              -> [(FNFrameElement, CompVP 'PH1)]
matchPrepArgs x'tr rolemap cp patt felst = do
  (p,(prep,mising)) <- pbArgForPP patt
  role <- FNFrameElement <$> maybeToList (lookup p rolemap)
  pp <- maybeToList (matchPP x'tr cp (Just prep,Nothing,mising))
  let rng_pp = pp^.maximalProjection
      comp_pp = CompVP_PP rng_pp
  guard (is _Nothing (find (\x -> x^?_2.to (compVPToRange SPH1) == Just rng_pp) felst))
  return (role, comp_pp)


matchAgentForPassive :: X'Tree 'PH1
                     -> [(PBArg,Text)]
                     -> CP 'PH1
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, CompVP 'PH1)
matchAgentForPassive x'tr rolemap cp patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    pp  <- matchPP x'tr cp (Just "by",Just PC_Other,Nothing)
    let comp = CompVP_PP (pp^.maximalProjection)
    (,comp) . FNFrameElement <$> lookup p rolemap


matchSO :: X'Tree 'PH1
        -> [(PBArg,Text)]
        -> (Maybe (SpecTP 'PH1),VerbP 'PH1,CP 'PH1)
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, CompVP 'PH1)])
matchSO x'tr rolemap (mDP,verbp,cp) (patt,num) =
  let rmatched0 = case verbp^.headX.vp_voice of
                    Active  -> maybeToList (matchSubject x'tr rolemap mDP patt) ++ matchObjects x'tr rolemap verbp patt
                    Passive -> maybeToList (matchAgentForPassive x'tr rolemap cp patt) ++ matchObjects x'tr rolemap verbp patt
      rmatched1 = rmatched0 ++ maybeToList (matchExtraRolesForPPTime x'tr cp rmatched0)
  in ((patt,num),rmatched1 ++ matchPrepArgs x'tr rolemap cp patt rmatched1)


extendRoleMapForDual :: (FNFrame,SenseID,[(PBArg,Text)])
                     -> (FNFrame,SenseID,[(PBArg,Text)])
extendRoleMapForDual (frame,sense,rolemap) = fromMaybe (frame,sense,rolemap) $ do
  dualmap <- lookup frame $ map (\c -> (c^.cm_baseFrame,c)) causeDualMap
  let frame' = dualmap^.cm_causativeFrame
      rolemap' = filter (\(k,_v) -> k /= "frame" && k /= "arg0") rolemap
      rolemap'' =  map f rolemap'
        where f (k,v) = maybe (k,v) ((k,).unFNFrameElement) (lookup (FNFrameElement v) (dualmap^.cm_extraMapping))
      rolemap''' = ("arg0",unFNFrameElement (dualmap^.cm_externalAgent)) : rolemap''
  return (frame',sense,rolemap''')


numMatchedRoles :: ((ArgPattern () GRel, Int), [(FNFrameElement, a)]) -> Int
numMatchedRoles = lengthOf (_2.folded)


matchRoles :: X'Tree 'PH1
           -> [(PBArg,Text)]
           -> VerbP 'PH1
           -> CP 'PH1
           -> [(ArgPattern () GRel, Int)]
           -> Maybe (SpecTP 'PH1)
           -> Maybe ((ArgPattern () GRel, Int),[(FNFrameElement, CompVP 'PH1)])
matchRoles x'tr rolemap verbp cp toppattstats mDP =
    (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy (flip compare `on` numMatchedRoles)) matched
  where
    matched = map (matchSO x'tr rolemap (mDP,verbp,cp)) toppattstats
    cmpstat  = flip compare `on` (^._1._2)
    eq       = (==) `on` lengthOf (_2.folded)



matchFrameRolesForCauseDual :: X'Tree 'PH1
                            -> VerbP 'PH1
                            -> CP 'PH1
                            -> [(ArgPattern () GRel,Int)]
                            -> Maybe (SpecTP 'PH1)
                            -> LittleV
                            -> ([Text], FNFrame, SenseID, [(PBArg, Text)])
                            -> ([Text], FNFrame, (SenseID,Bool) , Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP 'PH1)]))
matchFrameRolesForCauseDual x'tr verbp cp toppatts mDP causetype (idiom, frame1,sense1,rolemap1) =
  let (frame2,sense2,rolemap2) = if causetype == LVDual
                                 then extendRoleMapForDual (frame1,sense1,rolemap1)
                                 else (frame1,sense1,rolemap1)
      mselected1 = matchRoles x'tr rolemap1 verbp cp toppatts mDP
      mselected2 = matchRoles x'tr rolemap2 verbp cp toppatts mDP
  in case (mselected1,mselected2) of
       (Nothing,Nothing) -> (idiom, frame1,(sense1,False),Nothing)
       (Just _ ,Nothing) -> (idiom, frame1,(sense1,False),mselected1)
       (Nothing,Just _ ) -> (idiom, frame2,(sense2,True),mselected2)
       (Just s1,Just s2) ->
         case (compare `on` numMatchedRoles) s1 s2 of
           GT -> (idiom, frame1,(sense1,False),mselected1)
           LT -> (idiom, frame2,(sense2,True),mselected2)
           EQ -> (idiom, frame1,(sense1,False),mselected1)   -- choose intransitive because transitive should
                                                      -- have one more argument in general.


matchFrameRolesAll :: X'Tree 'PH1
                   -> VerbP 'PH1
                   -> CP 'PH1
                   -> Maybe (SpecTP 'PH1)
                   -> [(([Text],RoleInstance,Int),[(ArgPattern () GRel,Int)])]
                   -> [(([Text],FNFrame,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP 'PH1)])),Int)]
matchFrameRolesAll x'tr verbp cp mDP rmtoppatts = do
  (rm,toppatts) <- rmtoppatts
  let sense1 = rm^._2._1
      rolemap1 = rm^._2._2
      stat = rm^._3
      idiom = rm^._1
  frame1 <- FNFrame <$> maybeToList (lookup "frame" rolemap1)
  causetype <- (\x -> if x == "dual" then LVDual else LVSingle) <$> maybeToList (lookup "cause" rolemap1)
  return (matchFrameRolesForCauseDual x'tr verbp cp toppatts mDP causetype (idiom,frame1,sense1,rolemap1),stat)


matchPPObjectRange x'tr felst rng =
  flip find felst $ \x -> fromMaybe False $ do
    rng1 <- x^?_2._CompVP_PP
    pp1 <- extractZipperById rng1 x'tr >>= \w -> currentCPDPPP w ^? _PPCase
    let rng'  = pp1^.complement.to (compPPToRange SPH1)
    return (rng' == rng)


matchExtraRolesForPPTime :: X'Tree 'PH1
                         -> CP 'PH1
                         -> [(FNFrameElement, CompVP 'PH1)]
                         -> Maybe (FNFrameElement,CompVP 'PH1)
matchExtraRolesForPPTime x'tr cp felst = do
  guard (is _Nothing (find (\x -> x^._1 == "Time" || x^._1 == "Duration") felst))
  pp <- matchPP x'tr cp (Nothing,Just PC_Time,Just False)
  let rng_pp = pp^.maximalProjection
      comp = CompVP_PP rng_pp
      matched = matchPPObjectRange x'tr felst (pp^.complement.to (compPPToRange SPH1))
        {- flip find felst $ \x -> fromMaybe False $ do
        rng1 <- x^?_2._CompVP_PP
        pp1 <- extractZipperById rng1 x'tr >>= \w -> currentCPDPPP w ^? _PPCase
        let rng'  = pp1^.complement.to (compPPToRange SPH1)
            rng'' = pp^.complement.to (compPPToRange SPH1)
        return (rng' == rng'') -}
        -- .complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst)
  guard (is _Nothing matched)
  ((do prep <- pp^?headX.hp_prep._Prep_WORD
       find (\x -> (x^._1 == prep) && ("Duration" `elem` x^._2)) ppExtraRoleMap
       return ("Duration",comp)
    )
   <|> return ("Time",comp))



matchExtraRolesForGenericPP :: [Text]
                            -> X'Tree 'PH1
                            -> CP 'PH1
                            -> [(FNFrameElement, CompVP 'PH1)]
                            -> Maybe (FNFrameElement,CompVP 'PH1)
matchExtraRolesForGenericPP fes x'tr cp felst = do
  let fes0 = map (^._1) felst
  pp <- matchPP x'tr cp (Nothing,Just PC_Other,Just False)
  prep <- pp^?headX.hp_prep._Prep_WORD
  let roles = ppExtraRoles prep
  role <- find (\r -> r /= "Time" && r /= "Duration" && unFNFrameElement r `elem` fes && not (r `elem` fes0)) roles
  let matched = matchPPObjectRange x'tr felst (pp^.complement.to (compPPToRange SPH1))
  guard (is _Nothing matched)
  -- guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  let comp = CompVP_PP (pp^.maximalProjection)
  return (role,comp)


matchExtraRolesForPPing :: Text
                        -> FNFrameElement
                        -> X'Tree 'PH1
                        -> CP 'PH1
                        -> [(FNFrameElement, CompVP 'PH1)]
                        -> Maybe (FNFrameElement,CompVP 'PH1)
matchExtraRolesForPPing prep role x'tr cp felst = do
  guard (isNothing (find (\x -> x^._1 == role) felst))
  pp <- matchPP x'tr cp (Just prep,Just PC_Other,Just True)
  let matched = matchPPObjectRange x'tr felst (pp^.complement.to (compPPToRange SPH1))
  guard (is _Nothing matched)

  -- guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  let comp = CompVP_PP (pp^.maximalProjection)
  return (role,comp)


matchExtraRolesForCPInCompVP :: (CP 'PH1 -> Bool)
                             -> FNFrameElement
                             -> X'Tree 'PH1
                             -> CP 'PH1
                             -> [(FNFrameElement, CompVP 'PH1)]
                             -> Maybe (FNFrameElement,CompVP 'PH1)
matchExtraRolesForCPInCompVP check role x'tr cp0 felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let resmap = retrieveResolved x'tr
  let candidates = do
        c <- cp0 ^.complement.complement.complement
        CompVP_CP rng_cp <- maybeToList (resolvedCompVP resmap c)
        cp <- maybeToList (extractZipperById rng_cp x'tr >>= \w -> currentCPDPPP w ^? _CPCase)
        return (rng_cp,cp)
  (rng_cp,cp) <- find (check.snd) candidates

  -- let rng_cp = cp^.maximalProjection
  let matched = matchPPObjectRange x'tr felst rng_cp
  guard (is _Nothing matched)
  -- guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng_cp) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP == Just rng_cp) felst))
  let comp = CompVP_CP rng_cp
  return (role,comp)


matchExtraRolesForCPInAdjunctCP :: (CP 'PH1 -> Bool)
                                -> FNFrameElement
                                -> X'Tree 'PH1
                                -> CP 'PH1
                                -> [(FNFrameElement, CompVP 'PH1)]
                                -> Maybe (FNFrameElement,CompVP 'PH1)
matchExtraRolesForCPInAdjunctCP check role x'tr cp0 felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let resmap = retrieveResolved x'tr
  let candidates = do
        rng_cp <- cp0^..adjunct.traverse._AdjunctCP_CP
        cp <- maybeToList (extractZipperById rng_cp x'tr >>= \w -> currentCPDPPP w ^? _CPCase)
        return (rng_cp,cp)

  (rng_cp,cp) <- find (check.snd) candidates
  -- let rng_cp = cp^.maximalProjection
  -- guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng_cp) felst))
  let matched = matchPPObjectRange x'tr felst rng_cp
  guard (is _Nothing matched)
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP == Just rng_cp) felst))
  let comp = CompVP_CP rng_cp
  return (role,comp)


hasComplementizer :: [Lemma] -> CP 'PH1 -> Bool
hasComplementizer lst x = x^?headX._C_WORD.to (\z -> any (== z) lst) == Just True


toInfinitive :: CP 'PH1 -> Bool
toInfinitive x =
  let vprop = x^.complement.complement.headX
  in case vprop^.vp_auxiliary of
       (_,(_,lma)):_ -> lma == "to"
       _             -> False


--
-- | Match common extra-thematic roles such as Means, Purpose, Explanation and Time_vector
--   This function should be generalized.
--
matchExtraRoles :: FrameDB
                -> FNFrame
                -> X'Tree 'PH1
                -> CP 'PH1
                -> [(FNFrameElement, CompVP 'PH1)]
                -> [(FNFrameElement, CompVP 'PH1)]
matchExtraRoles frmdb frame x'tr cp felst =
  let mmeans = matchExtraRolesForPPing "by" "Means" x'tr cp felst
      felst1 = felst ++ maybeToList mmeans
      mcomp  = matchExtraRolesForCPInCompVP toInfinitive "Purpose" x'tr cp felst1 <|>
               matchExtraRolesForPPing "after"  "Time_vector" x'tr cp felst1 <|>
               matchExtraRolesForPPing "before" "Time_vector" x'tr cp felst1
      felst2 = felst1 ++ maybeToList mcomp
      madj   = matchExtraRolesForCPInAdjunctCP (hasComplementizer ["as"]) "Explanation" x'tr cp felst2 <|>
               matchExtraRolesForCPInAdjunctCP toInfinitive               "Purpose"     x'tr cp felst2 <|>
               matchExtraRolesForCPInAdjunctCP (not.hasComplementizer ["after","before","as","while","if","though","although","unless"]) "Event_description" x'tr cp felst2 --for the time being
      felst3 = felst2 ++ maybeToList madj
      fes = do frm <- maybeToList (HM.lookup (unFNFrame frame) (frmdb^.frameDB))
               frm^..frame_FE.traverse.fe_name
      madjpp = matchExtraRolesForGenericPP fes x'tr cp felst3
  in felst3 ++ maybeToList madjpp


--
-- | Match extra clause modifier subframes
--
matchExtraClausalSubframe :: (CP 'PH1 -> Bool)
                          -> Text
                          -> (FNFrame,FNFrameElement,FNFrameElement)
                          -> X'Tree 'PH1
                          -> CP 'PH1
                          -> Maybe (FNFrame,Text,[(FNFrameElement,(Bool,Range))])
matchExtraClausalSubframe check prep (frm,fe0,fe1) x'tr cp0 = do
  let rng0 = cp0^.maximalProjection
  let resmap = retrieveResolved x'tr
  let candidates1 = do
        c <- cp0 ^.complement.complement.complement
        CompVP_CP rng_cp <- maybeToList (resolvedCompVP resmap c)
        cp <- maybeToList (extractZipperById rng_cp x'tr >>= \w -> currentCPDPPP w ^? _CPCase)
        return (rng_cp,cp)
      candidates2 = do
        rng_cp <- cp0^..adjunct.traverse._AdjunctCP_CP
        cp <- maybeToList (extractZipperById rng_cp x'tr >>= \w -> currentCPDPPP w ^? _CPCase)
        return (rng_cp,cp)
      candidates = candidates1 ++ candidates2
  (rng1,cp1) <- find (check.snd) candidates
  -- let rng1 = cp1^.maximalProjection
  return (frm,prep,[(fe0,(True,rng0)),(fe1,(False,rng1))])


--
-- | A scoring algorithm for selecting a frame among candidates.
--   This version is ad hoc, so it will be updated when we come up with a better algorithm.
--
scoreSelectedFrame :: Int
                   -> (([Text],FNFrame,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement,a)])),Int)
                   -> Double
scoreSelectedFrame total ((_,_,_,mselected),n) =
  let mn = maybe 0 fromIntegral (mselected^?_Just.to numMatchedRoles)
  in mn * (fromIntegral n) / (fromIntegral total) * roleMatchWeightFactor + (mn*(fromIntegral total))



{-
--
-- | Resolve PP ambiguity for matched PP
--   This algorithm is ad hoc but practically works for PP embedded in DP by CoreNLP.
--   We need to have a systematic treatment and tests for PP ambiguity.
--
resolveAmbiguityInDP :: [(FNFrameElement, CompVP 'PH1)]
                     -> [(FNFrameElement, CompVP 'PH1)]
resolveAmbiguityInDP [] = []
resolveAmbiguityInDP felst = foldr1 (.) (map go felst) felst
  where
    go :: (FNFrameElement,CompVP 'PH1)
       -> [(FNFrameElement,CompVP 'PH1)]
       -> [(FNFrameElement,CompVP 'PH1)]
    go (fe,CompVP_PP pp) lst = let rng = pp^.maximalProjection
                               in map (f (fe,rng)) lst
    go (fe,CompVP_DP dp) lst = map (f (fe,fromMaybe (dp^.maximalProjection) (dp^?complement._Just.headX.hn_range))) lst
    go (_ ,_           ) lst = lst

    f (fe,rng@(b,e)) (fe',CompVP_PP pp)
      = let HeadPP p1 p2 = pp^.headX
            prep' = (p1,p2)
            o'    = pp^.maximalProjection
        in case pp^.complement of
             CompPP_DP dp    -> let rng'@(b',_) = dp^.maximalProjection
                                in -- for the time being, use this ad hoc algorithm
                                   if fe /= fe' && rng `isInsideR` rng' && b /= b'
                                   then let dp' = dp & (complement._Just.headX.hn_range .~  (b',b-1))
                                                     . (complement._Just.maximalProjection .~ (b,e))
                                                     . (maximalProjection .~ (b,e))
                                                     . (adjunct .~ [])
                                        in (fe',CompVP_PP (mkPP prep' o' dp'))
                                   else (fe',CompVP_PP pp)
             CompPP_Gerund _ -> (fe',CompVP_PP pp)
    f (fe,rng@(b,e)) (fe',CompVP_DP dp)
      = let rng'@(b',_) = dp^.maximalProjection
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let dp' = dp & (complement._Just.headX.hn_range .~ (b',b-1))
                            . (complement._Just.maximalProjection .~ (b,e))
                            . (maximalProjection .~ (b,e))
                            . (adjunct .~ [])
               in (fe', CompVP_DP dp')
          else (fe', CompVP_DP dp)
    f _ x = x
-}


matchFrame :: FrameDB
           -> X'Tree 'PH1
           -> (VerbStructure,CP 'PH1)
           -> Maybe (Range,VerbProperty (Zipper '[Lemma]),X'Tree 'PH1,FrameMatchResult,Maybe (SenseID,Bool))
matchFrame frmdb x'tr (vstr,cp) =
    if verbp^.headX.vp_lemma == "be"
    then do
      rng_dp_subj <- mDP^?_Just._SpecTP_DP   -- for the time being, ignore CP subject
      c <- listToMaybe $ do
             c <- verbp^.complement
             maybeToList (resolvedCompVP resmap c)

      -- c <- listToMaybe (verbp^..complement.traverse.trResolved._Just)
      ((do rng_dp_obj <- c^?_CompVP_DP
           let argpatt = ArgPattern Nothing (Just (GR_NP (Just GASBJ))) (Just (GR_NP (Just GA1))) Nothing Nothing Nothing
               role_subj = (FNFrameElement "Instance",CompVP_DP rng_dp_subj)
               role_obj  = (FNFrameElement "Type",CompVP_DP rng_dp_obj)
           return (rng_cp,vprop,x'tr,FMR ["be"] "Instance" (Just ((argpatt,1),[role_subj,role_obj])) [],Nothing))
       <|>
       (do rng_pp_obj <- c^?_CompVP_PP
           pp_obj <- extractZipperById rng_pp_obj x'tr >>= \w -> currentCPDPPP w ^? _PPCase
           prep_obj <- pp_obj^?headX.hp_prep._Prep_WORD
           (frm,rsbj,robj) <- ppRelFrame prep_obj
           let argpatt = ArgPattern Nothing (Just (GR_NP (Just GASBJ))) (Just (GR_PP Nothing)) Nothing Nothing Nothing
               role_subj = (rsbj,CompVP_DP rng_dp_subj)
               role_obj  = (robj,CompVP_PP rng_pp_obj)
           return (rng_cp,vprop,x'tr,FMR ["be"] frm (Just ((argpatt,1),[role_subj,role_obj])) [],Nothing)))


    else do
      ((idiom,frame,sense,mselected0),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)
      let mselected = (_Just . _2 %~ matchExtraRoles frmdb frame x'tr cp) mselected0
          -- mselected  = mselected1 -- (_Just . _2 %~ resolveAmbiguityInDP) mselected1
          subfrms = mapMaybe (\(chk,prep,frm) -> matchExtraClausalSubframe chk prep frm x'tr cp)
                      [(hasComplementizer ["after"] , "after" , ("Time_vector","Event","Landmark_event"))
                      ,(hasComplementizer ["before"], "before", ("Time_vector","Event","Landmark_event"))
                      ,(hasComplementizer ["while"] , "while" , ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                      ,(hasComplementizer ["though","although"], "though", ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                      ,(hasComplementizer ["if"]    , "if"    , ("Conditional_occurrence","Consequence","Profiled_possibility"))
                      ,(hasComplementizer ["unless"], "unless", ("Negative_conditional","Anti_consequence","Profiled_possibility"))
                      ]
      return (rng_cp,vprop,x'tr,FMR idiom frame mselected subfrms,Just sense)
  where
    resmap = retrieveResolved x'tr

    verbp = cp^.complement.complement
    mDP = do
      let s = cp^.complement.specifier -- .trResolved
      -- SpecTP_DP rng_dp <-
      resolvedSpecTP resmap s
      -- extractZipperById rng_dp x'tr >>= \w -> currentCPDPPP w ^? _DPCase

    vprop = vstr^.vs_vp
    rng_cp = cp^.maximalProjection
    frmsels = matchFrameRolesAll x'tr verbp cp mDP (vstr^.vs_roleTopPatts)
    total=  sum (frmsels^..traverse._2)



--
-- | Normalization of verb. Necessary for American-British English normalization.
--   This was critical when finding the original form of the verb from deverbalized noun.
--   For example, `criticism` is recognized as a derived noun form from the verb `criticise`
--   from WordNet, but in PropBank, we have only `criticize` in American English -- from Wall Street Journal.
--   Until we have a generic morphology mapping between American and British English, we should designate each
--   case by explicit rules.
--
renormalizeVerb :: Text -> Text
renormalizeVerb vlma =
  let n = T.length vlma
      (ini,rest) = T.splitAt (n-3) vlma
  in if rest == "ise" && n > 5 then ini<>"ize" else vlma


extractNominalizedVerb :: WordNetDB -> Lemma -> [Lemma]
extractNominalizedVerb wndb (Lemma lma) =
  let verbs = (map head . group . sort) $ do
        (_,_,xs,ptrs,_) <- lookupLemma wndb POS_N lma
        (_,lst) <- getDerivations wndb lma (xs,ptrs)
        (_,((pos,_),li_v)) <- lst
        guard (pos == POS_V)
        let vlma = renormalizeVerb (li_v^.lex_word)
        return (Lemma vlma)
  in verbs

--
-- | This is a simple utility function to extract subject and object only from a given verb subcategorization pattern.
--   We use this for the following function `matchNomFrame` which identifies nominal frame by matching SpecDP to the
--   subject and CompDP to the object of the corresponding verb to a given deverbalized noun.
--
subjObjNP :: ArgPattern () GRel -> (Maybe (Text,GRel), Maybe (Text,GRel))
subjObjNP argpatt =
  let m = catMaybes [ ("arg0",) <$> argpatt^.patt_arg0
                    , ("arg1",) <$> argpatt^.patt_arg1
                    , ("arg2",) <$> argpatt^.patt_arg2
                    , ("arg3",) <$> argpatt^.patt_arg3
                    , ("arg4",) <$> argpatt^.patt_arg4 ]
  in (find (\(_,p) -> p == GR_NP (Just GASBJ)) m, find (\(_,p) -> p == GR_NP (Just GA1)) m)


subjObjSBAR :: ArgPattern () GRel -> (Maybe (Text,GRel), Maybe (Text,GRel))
subjObjSBAR argpatt =
  let m = catMaybes [ ("arg0",) <$> argpatt^.patt_arg0
                    , ("arg1",) <$> argpatt^.patt_arg1
                    , ("arg2",) <$> argpatt^.patt_arg2
                    , ("arg3",) <$> argpatt^.patt_arg3
                    , ("arg4",) <$> argpatt^.patt_arg4 ]
  in (find (\(_,p) -> p == GR_NP (Just GASBJ)) m, find (\(_,p) -> p == GR_SBAR (Just GA1)) m)



matchNomFrame :: AnalyzePredata
              -> X'Tree 'PH1
              -> PreAnalysis '[Lemma]
              -> DetP 'PH1
              -> Maybe (Lemma,Lemma,X'Tree 'PH1,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))
matchNomFrame apredata x'tr tagged dp = do
    let rng_dp = dp^.maximalProjection
        wndb = apredata^.analyze_wordnet
    (b,e) <- dp^?complement._Just.headX.coidx_content.hn_range
    -- for the time being, treat only single word nominal as frame
    guard (b==e)
    -- For the time being, I identify nominal frame only for a noun phrase with of, or with to-infinitive.
    lma <- listToMaybe (tagged^..lemmaList.folded.filtered (^._1.to (\i -> i == b))._2._1)
    let mei_subj :: Maybe EntityInfo
        mei_subj = do
          rng <- case dp^.headX.hd_class of
                   Pronoun _pperson True -> dp^.headX.hd_range
                   GenitiveClitic -> let specs :: [SpecDP]
                                         specs = dp^.specifier
                                         rngs :: [Range]
                                         rngs = mapMaybe (^?_SpDP_Gen) specs
                                     in listToMaybe rngs
                   _ -> Nothing
          let txt = T.intercalate " " (tokensByRange tagged rng)
          return (EI rng rng Nothing txt False False)
    (verb,_senses,rmtoppatts) <- getFirst . mconcat $ do
      verb <- extractNominalizedVerb wndb lma
      let (senses,rmtoppatts) = getVerbSenses apredata (verb,[verb])
      guard ((not.null) senses && (not.null) rmtoppatts)
      (return . First . Just) (verb,senses,rmtoppatts)
    ((_,(_sid,rolemap),_),patts) <- listToMaybe (sortBy (flip compare `on` (^._1._3) ) rmtoppatts)   -- choose most frequent
    frm <- lookup "frame" rolemap
    patt <- listToMaybe patts
    (ppcase patt rolemap lma frm verb rng_dp mei_subj <|>
     cpcase patt rolemap lma frm verb rng_dp mei_subj)
  where
    ppcase patt rolemap lma frm verb rng_dp mei_subj = do
      let (ms,mo) = subjObjNP (patt^._1)
      (args,_) <- ms
      (argo,_) <- mo
      subj <- FNFrameElement <$> lookup args rolemap
      obj  <- FNFrameElement <$> lookup argo rolemap
      rng_pp <- dp^?complement._Just.complement._Just._CompDP_PP
      pp <- cpdpppFromX'Tree x'tr rng_pp _PPCase
      guard (pp^.headX.hp_prep == Prep_WORD "of")
      rng_obj <- pp^?complement._CompPP_DP -- .maximalProjection
      let txt_obj = T.intercalate " " (tokensByRange tagged rng_obj)
      return (lma,verb,x'tr,(FNFrame frm,rng_dp),(subj,mei_subj),(obj,(EI rng_obj rng_obj (Just "of") txt_obj False False)))
    cpcase patt rolemap lma frm verb rng_dp mei_subj = do
      let (ms,mo) = subjObjSBAR (patt^._1)
      (args,_) <- ms
      (argo,_) <- mo
      subj <- FNFrameElement <$> lookup args rolemap
      obj  <- FNFrameElement <$> lookup argo rolemap
      compdp <- dp^?complement._Just.complement._Just
      cp_obj <- case compdp of
                  CompDP_CP rng_obj -> cpdpppFromX'Tree x'tr rng_obj _CPCase
                  _ -> Nothing
      let vp = cp_obj^.complement.complement
          rng_obj = cp_obj^.maximalProjection
          txt_obj = T.intercalate " " (tokensByRange tagged rng_obj)
      aux <- listToMaybe (vp^..headX.vp_auxiliary.traverse._2._2.to unLemma)
      guard (aux == "to")
      return (lma,verb,x'tr,(FNFrame frm,rng_dp),(subj,mei_subj),(obj,(EI rng_obj rng_obj Nothing txt_obj False False)))
