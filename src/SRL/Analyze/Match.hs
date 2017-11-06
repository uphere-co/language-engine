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
import           Control.Monad                (guard)
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                  (First(..),(<>))
import qualified Data.Text              as T
import           Data.Text                    (Text)
--
import           Data.Bitree                  (getRoot1)
import           Data.BitreeZipper            (current,extractZipperById)
import           Data.Range                   (Range,elemRevIsInsideR,isInsideR)
import           Lexicon.Mapping.Causation    (causeDualMap,cm_baseFrame,cm_causativeFrame
                                              ,cm_externalAgent,cm_extraMapping)
import           Lexicon.Type
import           NLP.Syntax.Clause            (cpRange,constructCP,currentCPDPPP)
import           NLP.Syntax.Format.Internal   (formatCompVP,formatPP)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (GetIntLemma(..),isLemmaAs,intLemma0)
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
--
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,SentStructure,VerbStructure
                                              ,PredicateInfo(..)
                                              ,_PredNoun,_MGPredicate
                                              ,ss_cpstr,ss_tagged,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp
                                              ,me_relation,mv_range,mv_id,mg_vertices,mg_edges)
--
import Debug.Trace



mkTriples :: SentStructure -> ([X'Tree '[Lemma]],[(VerbStructure, CP '[Lemma])])
mkTriples sstr =
  let x'tr = sstr^.ss_cpstr
  in ( x'tr
     , [(vstr,cp)| vstr <- sstr ^.ss_verbStructures
                 , let vp = vstr^.vs_vp
                 , cp <- maybeToList $ do
                           let tagged = sstr^.ss_tagged
                           cp0 <- (^._1) <$> constructCP tagged vp
                           let rng = cpRange cp0
                           (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng)) x'tr)
                 ]
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


matchSubject :: [(PBArg,Text)]
             -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, CompVP '[Lemma])
matchSubject rolemap mDP patt = do
  dp <- mDP^?_Just._Right            -- for the time being
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,CompVP_DP dp) . FNFrameElement <$> lookup p rolemap


isPhiOrThat :: (GetIntLemma t) => CP t -> Bool
isPhiOrThat cp = case cp^.headX of
                   C_PHI -> True
                   C_WORD z -> isLemmaAs "that" (current z)



matchObjects :: [(PBArg,Text)]
             -> VerbP '[Lemma]
             -> ArgPattern p GRel
             -> [(FNFrameElement, CompVP '[Lemma])]
matchObjects rolemap verbp patt = do
  (garg,obj) <- zip [GA1,GA2] (filter (\case CompVP_CP _ -> True; CompVP_DP _ -> True; _ -> False) (verbp^..complement.traverse.trResolved._Just))
  (p,a) <- maybeToList (pbArgForGArg garg patt)
  case obj of
    CompVP_CP cp -> guard (isPhiOrThat cp && a == GR_SBAR (Just garg))
    CompVP_DP _  -> guard (a == GR_NP (Just garg))
    _            -> []
  fe <- FNFrameElement <$> maybeToList (lookup p rolemap)
  return (fe, obj)



matchPP :: CP '[Lemma]
        -> (Maybe Text,Maybe PrepClass,Maybe Bool)
        -> Maybe (PP '[Lemma])
matchPP cp (mprep,mpclass,mising) = do
    let compvps = cp^..complement.complement.complement.traverse.trResolved._Just
        candidates = do compvp <- compvps
                        case compvp of
                          CompVP_PP pp -> return pp
                          CompVP_DP dp -> dp^..adjunct.traverse._AdjunctDP_PP
                          _            -> []
    find ppcheck candidates
  where
    ppcheck pp = let (prep',pclass') = pp^.headX
                     ising' = is _Just (pp^?complement._CompPP_Gerund)
                 in maybe True (\prep -> Just prep == prep'^?_Prep_WORD) mprep &&
                    maybe True (== pclass') mpclass &&
                    maybe True (== ising') mising


matchPrepArgs :: [(PBArg,Text)]
              -> CP '[Lemma]
              -> ArgPattern p GRel
              -> [(FNFrameElement, CompVP '[Lemma])]
              -> [(FNFrameElement, CompVP '[Lemma])]
matchPrepArgs rolemap cp patt felst = do
  (p,(prep,mising)) <- pbArgForPP patt
  role <- FNFrameElement <$> maybeToList (lookup p rolemap)

  pp <- maybeToList (matchPP cp (Just prep,Nothing,mising))
  let comp = CompVP_PP pp
      rng = compVPToRange comp
  guard (is _Nothing (find (\x -> x^?_2.to compVPToRange == Just rng) felst))
  return (role, comp)


matchAgentForPassive :: [(PBArg,Text)]
                     -> CP '[Lemma]
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, CompVP '[Lemma])
matchAgentForPassive rolemap cp patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    pp  <- matchPP cp (Just "by",Just PC_Other,Nothing)
    let comp = CompVP_PP pp
    (,comp) . FNFrameElement <$> lookup p rolemap


matchSO :: [(PBArg,Text)]
        -> ( Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma])), VerbP '[Lemma], CP '[Lemma])
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, CompVP '[Lemma])])
matchSO rolemap (mDP,verbp,cp) (patt,num) =
  let (rpatt,rmatched0) = case verbp^.headX.vp_voice of
                            Active  -> ((patt,num), maybeToList (matchSubject rolemap mDP patt) ++ matchObjects rolemap verbp patt)
                            Passive -> ((patt,num),maybeToList (matchAgentForPassive rolemap cp patt) ++ matchObjects rolemap verbp patt)
      rmatched1 = rmatched0 ++ maybeToList (matchExtraRolesForPPTime cp rmatched0)
  in (rpatt,rmatched1 ++ matchPrepArgs rolemap cp patt rmatched1)


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


matchRoles :: [(PBArg,Text)]
           -> VerbP '[Lemma]
           -> CP '[Lemma]
           -> [(ArgPattern () GRel, Int)]
           -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
           -> Maybe ((ArgPattern () GRel, Int),[(FNFrameElement, CompVP '[Lemma])])
matchRoles rolemap verbp cp toppattstats mDP =
    (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy (flip compare `on` numMatchedRoles)) matched
  where
    matched = map (matchSO rolemap (mDP,verbp,cp)) toppattstats
    cmpstat  = flip compare `on` (^._1._2)
    eq       = (==) `on` lengthOf (_2.folded)



matchFrameRolesForCauseDual :: VerbP '[Lemma]
                            -> CP '[Lemma]
                            -> [(ArgPattern () GRel,Int)]
                            -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
                            -> LittleV
                            -> (FNFrame, SenseID, [(PBArg, Text)])
                            -> (FNFrame, (SenseID,Bool) , Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])]))
matchFrameRolesForCauseDual verbp cp toppatts mDP causetype (frame1,sense1,rolemap1) =
  let (frame2,sense2,rolemap2) = if causetype == LVDual
                                 then extendRoleMapForDual (frame1,sense1,rolemap1)
                                 else (frame1,sense1,rolemap1)
      mselected1 = matchRoles rolemap1 verbp cp toppatts mDP
      mselected2 = matchRoles rolemap2 verbp cp toppatts mDP
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


matchFrameRolesAll :: VerbP '[Lemma]
                   -> CP '[Lemma]
                   -> Maybe (Either (Zipper '[Lemma]) (DetP '[Lemma]))
                   -> [((RoleInstance,Int),[(ArgPattern () GRel,Int)])]
                   -> [((FNFrame,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])])),Int)]
matchFrameRolesAll verbp cp mDP rmtoppatts = do
  (rm,toppatts) <- rmtoppatts
  let sense1 = rm^._1._1
      rolemap1 = rm^._1._2
      stat = rm^._2
  frame1 <- FNFrame <$> maybeToList (lookup "frame" rolemap1)
  causetype <- (\x -> if x == "dual" then LVDual else LVSingle) <$> maybeToList (lookup "cause" rolemap1)
  return (matchFrameRolesForCauseDual verbp cp toppatts mDP causetype (frame1,sense1,rolemap1),stat)


matchExtraRolesForPPTime :: CP '[Lemma]
                         -> [(FNFrameElement, CompVP '[Lemma])]
                         -> Maybe (FNFrameElement,CompVP '[Lemma])
matchExtraRolesForPPTime cp felst = do
  guard (isNothing (find (\x -> x^._1 == "Time") felst))
  pp <- matchPP cp (Nothing,Just PC_Time,Just False)
  let comp = CompVP_PP pp
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  return ("Time",comp)


matchExtraRolesForPPing :: Text
                        -> FNFrameElement
                        -> CP '[Lemma]
                        -> [(FNFrameElement, CompVP '[Lemma])]
                        -> Maybe (FNFrameElement,CompVP '[Lemma])
matchExtraRolesForPPing prep role cp felst = do
  guard (isNothing (find (\x -> x^._1 == role) felst))
  pp <- matchPP cp (Just prep,Just PC_Other,Just True)
  let comp = CompVP_PP pp
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  return (role,comp)


matchExtraRolesForCPInCompVP :: (CP '[Lemma] -> Bool)
                             -> FNFrameElement
                             -> CP '[Lemma]
                             -> [(FNFrameElement, CompVP '[Lemma])]
                             -> Maybe (FNFrameElement,CompVP '[Lemma])
matchExtraRolesForCPInCompVP check role cp0 felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let candidates = cp0 ^..complement.complement.complement.traverse.trResolved._Just._CompVP_CP
  cp <- find check candidates
  let rng = cp^.maximalProjection.to current.to getRange
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.to cpRange == Just rng) felst))
  let comp = CompVP_CP cp
  return (role,comp)


matchExtraRolesForCPInAdjunctCP :: (CP '[Lemma] -> Bool)
                                -> FNFrameElement
                                -> CP '[Lemma]
                                -> [(FNFrameElement, CompVP '[Lemma])]
                                -> Maybe (FNFrameElement,CompVP '[Lemma])
matchExtraRolesForCPInAdjunctCP check role cp0 felst = do
  guard (is _Nothing (find (\x -> x^._1 == role) felst))
  let candidates = cp0^..adjunct.traverse._AdjunctCP_CP
  cp <- find check candidates
  let rng = cp^.maximalProjection.to current.to getRange
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.to cpRange == Just rng) felst))
  let comp = CompVP_CP cp
  return (role,comp)


hasComplementizer :: [Lemma] -> CP '[Lemma] -> Bool
hasComplementizer lst x =
  x^?headX._C_WORD.to (\z -> any (\c -> isLemmaAs c (current z)) lst) == Just True


toInfinitive :: CP '[Lemma] -> Bool
toInfinitive x =
  let vprop = x^.complement.complement.headX
  in case vprop^.vp_auxiliary of
       (_,(_,lma)):_ -> lma == "to"
       _             -> False


-- | this function should be generalized.
--
matchExtraRoles :: CP '[Lemma]
                -> [(FNFrameElement, CompVP '[Lemma])]
                -> [(FNFrameElement, CompVP '[Lemma])]
matchExtraRoles cp felst =
  let mmeans = matchExtraRolesForPPing "by" "Means" cp felst
      felst' = felst ++ maybeToList mmeans
      mcomp  = matchExtraRolesForCPInCompVP toInfinitive "Purpose" cp felst' <|>
               matchExtraRolesForPPing "after"  "Time_vector" cp felst' <|>
               matchExtraRolesForPPing "before" "Time_vector" cp felst'
      felst'' = felst' ++ maybeToList mcomp
      madj   = matchExtraRolesForCPInAdjunctCP (hasComplementizer ["as"]) "Explanation" cp felst'' <|>
               matchExtraRolesForCPInAdjunctCP toInfinitive               "Purpose"     cp felst'' <|>
               matchExtraRolesForCPInAdjunctCP (not.hasComplementizer ["after","before","as","while","if","though","although","unless"]) "Event_description" cp felst'' --for the time being
  in felst'' ++ maybeToList madj


matchSubFrame :: (CP '[Lemma] -> Bool)
              -> Text
              -> (FNFrame,FNFrameElement,FNFrameElement)
              -> CP '[Lemma]
              -> Maybe (FNFrame,Text,[(FNFrameElement,(Bool,Range))])
matchSubFrame check prep (frm,fe0,fe1) cp0 = do
  let rng0 = cp0^.maximalProjection.to current.to getRange
  let candidates = cp0 ^..complement.complement.complement.traverse.trResolved._Just._CompVP_CP ++ cp0^..adjunct.traverse._AdjunctCP_CP
  cp1 <- find check candidates
  let rng1 = cp1^.maximalProjection.to current.to getRange
  return (frm,prep,[(fe0,(True,rng0)),(fe1,(False,rng1))])


--
-- | A scoring algorithm for selecting a frame among candidates.
--   This version is ad hoc, so it will be updated when we come up with a better algorithm.
--
scoreSelectedFrame :: Int
                   -> ((FNFrame,(SenseID,Bool),Maybe ((ArgPattern () GRel,Int),[(FNFrameElement,a)])),Int)
                   -> Double
scoreSelectedFrame total ((_,_,mselected),n) =
  let mn = maybe 0 fromIntegral (mselected^?_Just.to numMatchedRoles)
  in mn * (fromIntegral n) / (fromIntegral total) * roleMatchWeightFactor + (mn*(fromIntegral total))


--
-- | Resolve PP ambiguity for matched PP
--   This algorithm is ad hoc but practically works for PP embedded in DP by CoreNLP.
--   We need to have a systematic treatment and tests for PP ambiguity.
--
resolveAmbiguityInDP :: [(FNFrameElement, CompVP '[Lemma])]
                     -> [(FNFrameElement, CompVP '[Lemma])]
resolveAmbiguityInDP [] = []
resolveAmbiguityInDP felst = foldr1 (.) (map go felst) felst
  where
    go :: (FNFrameElement,CompVP '[Lemma])
       -> [(FNFrameElement,CompVP '[Lemma])]
       -> [(FNFrameElement,CompVP '[Lemma])]
    go (fe,CompVP_PP pp) lst = let rng = pp^.maximalProjection
                               in map (f (fe,rng)) lst
    go (fe,CompVP_DP dp) lst = map (f (fe,dp^.headX)) lst
    go (_ ,_           ) lst = lst

    f (fe,rng@(b,e)) (fe',CompVP_PP pp)
      = let prep' = pp^.headX
            o'    = pp^.maximalProjection
        in case pp^.complement of
             CompPP_DP dp    -> let rng'@(b',_) = dp^.maximalProjection
                                in -- for the time being, use this ad hoc algorithm
                                   if fe /= fe' && rng `isInsideR` rng' && b /= b'
                                   then let dp' = ((headX .~  (b',b-1)) . (maximalProjection .~ (b,e)) . (adjunct .~ [])) dp
                                        in (fe',CompVP_PP (mkPP prep' o' dp'))
                                   else (fe',CompVP_PP pp)
             CompPP_Gerund _ -> (fe',CompVP_PP pp)
    f (fe,rng@(b,e)) (fe',CompVP_DP dp)
      = let rng'@(b',_) = dp^.maximalProjection
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let dp' = ((headX .~ (b',b-1)) . (maximalProjection .~ (b,e)) . (adjunct .~ [])) dp
               in (fe', CompVP_DP dp')
          else (fe', CompVP_DP dp)
    f _ x = x



matchFrame :: (VerbStructure,CP '[Lemma])
           -> Maybe (Range,VerbProperty (Zipper '[Lemma])
                    ,FNFrame
                    ,(SenseID,Bool)
                    ,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, CompVP '[Lemma])])
                    ,[(FNFrame,Text,[(FNFrameElement,(Bool,Range))])]
                    )
matchFrame (vstr,cp) = do
  let verbp = cp^.complement.complement
      mDP = cp^.complement.specifier.trResolved
      vprop = vstr^.vs_vp
      rng = cpRange cp
      frmsels = matchFrameRolesAll verbp cp mDP (vstr^.vs_roleTopPatts)
      total=  sum (frmsels^..traverse._2)
  ((frame,sense,mselected0),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)
  let mselected1 = (_Just . _2 %~ matchExtraRoles cp) mselected0
      mselected  = (_Just . _2 %~ resolveAmbiguityInDP) mselected1
      subfrms = mapMaybe (\(chk,prep,frm) -> matchSubFrame chk prep frm cp)
                  [(hasComplementizer ["after"] , "after" , ("Time_vector","Event","Landmark_event"))
                  ,(hasComplementizer ["before"], "before", ("Time_vector","Event","Landmark_event"))
                  ,(hasComplementizer ["while"] , "while" , ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                  ,(hasComplementizer ["though","although"], "though", ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                  ,(hasComplementizer ["if"]    , "if"    , ("Conditional_occurrence","Consequence","Profiled_possibility"))
                  ,(hasComplementizer ["unless"], "unless", ("Negative_conditional","Anti_consequence","Profiled_possibility"))
                  ]
  return (rng,vprop,frame,sense,mselected,subfrms)




dependencyOfX'Tree :: X'Tree p -> [(Range,Range)]
dependencyOfX'Tree (PN (rng0,_) xs) = map ((rng0,) . fst . getRoot1) xs ++ concatMap dependencyOfX'Tree xs
dependencyOfX'Tree (PL _)           = []



entityFromDP :: TaggedLemma t -> DetP t -> (Range,Text,Maybe (Range,Text))
entityFromDP tagged dp =
  let rng = dp^.headX
      headtxt = headText tagged dp
      txt = case dp^.complement of
              Just (CompDP_PP pp) ->
                let prep = pp^.headX._1
                    rng_pp = pp^.maximalProjection
                in if prep == Prep_WORD "of"
                   then headtxt <> " " <> T.intercalate " " (tokensByRange tagged rng_pp)
                   else headtxt
              _ -> headtxt
      mrngtxt' = do rng_sub <- dp^.specifier  -- for the time being, specifier is used as attribute appositive
                    let txt_sub = T.intercalate " " (tokensByRange tagged rng_sub)
                    return (rng_sub,txt_sub)
  in (rng,txt,mrngtxt')


meaningGraph :: SentStructure -> MeaningGraph
meaningGraph sstr =
  let (x'tr,lst_vstrcp) = mkTriples sstr
      tagged = sstr^.ss_tagged
      matched = mapMaybe matchFrame lst_vstrcp
      depmap = dependencyOfX'Tree =<< x'tr
      --
      preds = flip map matched $ \(rng,vprop,frame,sense,_mselected,_) i
                                   -> MGPredicate i (Just rng) frame (PredVerb sense (simplifyVProp vprop))
      ipreds = zipWith ($) preds [1..]
      --

      entities0 = do (_,_,_,_,mselected,_) <- matched
                     (_,felst) <- maybeToList mselected
                     (_fe,x) <- felst
                     case x of
                       CompVP_Unresolved _ -> []
                       CompVP_CP _cp -> [] -- CP is not an entity.
                       CompVP_DP dp -> (return . (_1 %~ Just) . entityFromDP tagged) dp
                       CompVP_PP pp -> maybeToList ((_1 %~ Just) . entityFromDP tagged <$> (pp^?complement._CompPP_DP))

      filterFrame = filter (\(rng,_,_) -> not (any (\p -> p^.mv_range == rng) ipreds))
      --

      entities1_0 = filterFrame
                  . map head
                  . groupBy ((==) `on` (^._1))
                  . sortBy (compare `on` (^._1))
                  $ entities0

      mkEntityFun (rng,txt,mrngtxt') =
        (\i -> MGEntity i rng txt []) :
          flip (maybe []) mrngtxt' (\(rng',txt') -> [ \i'  -> MGEntity i' (Just rng') txt' []
                                                    , \i'' -> MGPredicate i'' (Just rng') "Instance" PredNoun
                                                    ]
                                   )

      entities1 = concatMap mkEntityFun entities1_0

      entities2 = do (_,_,_,_,_,lst) <- matched
                     (frm,prep,felst) <- lst
                     -- (fe,rng) <- felst
                     return (\i -> ((i,frm,prep,felst),MGPredicate i Nothing frm (PredPrep prep)))
      n_ipreds = length ipreds
      n_entities1 = length entities1
      n_entities2 = length entities2
      ientities1 = zipWith ($) entities1 (enumFrom (n_ipreds+1))
      ientities2 = zipWith ($) entities2 (enumFrom (n_ipreds+n_entities1+1))
      vertices = ipreds ++ ientities1 ++ (map snd ientities2)
      --
      rangeid :: MGVertex -> (Int,Maybe Range)
      rangeid mv = (if mv^?_MGPredicate._4._PredNoun == Just () then 1 else 0, mv^.mv_range)
      --
      rngidxmap = HM.fromList [(rangeid v, v^.mv_id) | v <- vertices ]
      edges0 = do (rng,_,_,_,mselected,_) <- matched
                  i <- maybeToList (HM.lookup (0,Just rng) rngidxmap)   -- frame
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
                                    CompVP_DP dp -> return (dp^.headX,Nothing)
                                    CompVP_PP pp -> return (pp^.complement.to compPPToRange,pp^?headX._1._Prep_WORD)
                  i' <- maybeToList (HM.lookup (0,Just rng') rngidxmap)  -- frame element
                  let b = isJust (find (== (rng',rng)) depmap)
                  return (MGEdge fe b mprep i i')
      edges1 = do (mrng,_,mrngtxt') <- entities1_0
                  (rng',_) <- maybeToList mrngtxt'
                  i_frame <- maybeToList (HM.lookup (1,Just rng') rngidxmap)
                  i_instance <- maybeToList (HM.lookup (0,mrng) rngidxmap)
                  i_type     <- maybeToList (HM.lookup (0,Just rng') rngidxmap)
                  [MGEdge "Instance" True Nothing i_frame i_instance, MGEdge "Type" False Nothing i_frame i_type]
      edges2 = do (i_frame,frm,prep,felst) <- map fst ientities2
                  (fe,(b,rng)) <- felst
                  i_elem <- maybeToList (HM.lookup (0,Just rng) rngidxmap)
                  [MGEdge fe b Nothing i_frame i_elem]

  in MeaningGraph vertices (edges0 ++ edges1 ++ edges2)


isEntity :: MGVertex -> Bool
isEntity x = case x of
               MGEntity {..} -> True
               _             -> False


tagMG :: MeaningGraph -> [(Range,Text)] -> MeaningGraph
tagMG mg wikilst =
  let mg' = mg ^.. mg_vertices
                 . traverse
                 . to (\x -> case x^.mv_range of
                               Nothing -> x
                               Just rng -> if (rng `elemRevIsInsideR` map fst wikilst) && isEntity x
                                           then x { _mv_resolved_entities = map (^. _2) (filter (\w -> (w^._1) `isInsideR` rng) wikilst)}
                                           else x
                      )
  in MeaningGraph mg' (mg ^. mg_edges)


changeMGText :: MeaningGraph -> MeaningGraph
changeMGText mg =
  let mg' = mg ^.. mg_edges
                 . traverse
                 . to (\x -> x & (me_relation .~ (FNFrameElement . T.replace "&" "-AND-" . unFNFrameElement) (x ^. me_relation)))
      mg'' = mg ^.. mg_vertices
                  . traverse
                  . to (\x -> case x of
                           MGEntity {..} -> x { _mv_text = T.replace "&" "-AND-" _mv_text }
                           MGPredicate {..} -> x
                       )
  in MeaningGraph mg'' mg'
