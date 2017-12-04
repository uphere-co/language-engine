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
import           Data.BitreeZipper            (current,extractZipperById)
import           Data.Range                   (Range,isInsideR)
import           FrameNet.Query.Frame         (FrameDB,frameDB)
import           FrameNet.Type.Frame          (frame_FE,fe_name)
import           Lexicon.Mapping.Causation    (causeDualMap,cm_baseFrame,cm_causativeFrame
                                              ,cm_externalAgent,cm_extraMapping)
import           Lexicon.Type
import           NLP.Syntax.Clause            (constructCP,currentCPDPPP)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (GetIntLemma(..),isLemmaAs)
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
import           WordNet.Query                (WordNetDB,lookupLemma,getDerivations)
import           WordNet.Type                 (lex_word)
import           WordNet.Type.POS             (POS(..))
--
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Sense            (getVerbSenses)
import           SRL.Analyze.Type             (SentStructure,VerbStructure,AnalyzePredata(..)
                                              ,analyze_wordnet
                                              ,ss_x'tr,ss_tagged,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp)
import           SRL.Analyze.Type.Match       (EntityInfo(..),FrameMatchResult(..))
--
-- import Debug.Trace
-- import NLP.Syntax.Format.Internal


ppRelFrame :: Text -> Maybe (FNFrame,FNFrameElement,FNFrameElement)
ppRelFrame p = lookup p [ ("about"     , ("Topic"                        , "Text"     , "Topic"))
                        , ("above"     , ("Directional_locative_relation", "Figure"   , "Ground"))
                        , ("across"    , ("Distributed_position"         , "Theme"    , "Location"))
                        , ("after"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("against"   , ("Taking_sides"                 , "Cognizer" , "Issue"))
                        , ("along"     , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- alongside
                        , ("amid"      , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("amidst"    , ("Contrary_circumstances"       , "Event"    , "Adversity"))
                        , ("among"     , ("Be_subset_of"                 , "Part"     , "Total"))
                        , ("around"    , ("Distributed_position"         , "Theme"    , "Location"))
                        , ("as"        , ("Performers_and_roles"         , "Performer", "Role"))
                        , ("astride"   , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("at"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("atop"      , ("Spatial_contact"              , "Figure"   , "Ground"))
                        -- ontop
                        -- bar
                        , ("before"    , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("behind"    , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("below"     , ("Directional_locative_relation", "Figure"   , "Ground"))
                        , ("beneath"   , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("beside"    , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("besides"   , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("between"   , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("beyond"    , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- but
                        , ("by"        , ("Means"                        , "Purpose"  , "Means" ))
                        -- circa
                        -- come
                        , ("despite"   , ("Concessive"                   , "Main_assertion", "Conceded_state_of_affairs"))
                        , ("down"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("during"    , ("Temporal_collocation"         , "Trajector_event", "Landmark_event"))
                        -- except
                        , ("for"       , ("Purpose"                      , "Means"    , "Goal"))
                        , ("from"      , ("Origin"                       , "Entity"   , "Origin"))
                        , ("in"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("inside"    , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("into"      , ("Goal"                         , "Trajector", "Landmark"))
                        -- less
                        , ("like"      , ("Similarity"                   , "Entity_1" , "Entity_2"))
                        -- minus
                        , ("near"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- notwithstanding
                        , ("of"        , ("Partitive"                    , "Subset"   , "Group"))
                        , ("off"       , ("Spatial_contact"              , "Figure"   , "Ground"))
                        , ("on"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- onto
                        -- opposite
                        , ("out"       , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("outside"   , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("over"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("past"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- per
                        -- save
                        -- short
                        , ("since"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        -- than
                        , ("through"   , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("througout" , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("to"        , ("Goal"                         , "Trajector", "Landmark"))
                        , ("toward"    , ("Goal"                         , "Trajector", "Landmark"))
                        , ("towards"   , ("Goal"                         , "Trajector", "Landmark"))
                        , ("under"     , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("underneath", ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("unlike"    , ("Similarity"                   , "Entity_1" , "Entity_2"))
                        , ("until"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("till"      , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("up"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("upon"      , ("Spatial_contact"              , "Figure"   , "Ground"))
                        -- upside
                        -- versus
                        -- via
                        , ("with"      , ("Accompaniment"                , "Participant","Co-participant"))
                        , ("within"    , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("without"   , ("Negation"                     , "Factual_situation", "Negated_proposition"))
                        -- worth
                        ]



ppExtraRoles :: Text -> [FNFrameElement]
ppExtraRoles p = join (maybeToList (lookup p ppExtraRoleMap))


ppExtraRoleMap :: [(Text,[FNFrameElement])]
ppExtraRoleMap = [ ("about"     , ["Topic"])
                 , ("above"     , ["Location"])
                 , ("across"    , ["Path"])
                 , ("after"     , ["Time"])
                 , ("against"   , ["Issue"])
                 , ("along"     , ["Path"])
                 -- alongside
                 , ("amid"      , ["Duration"])
                 , ("amidst"    , ["Duration"])
                 , ("among"     , ["Total","Possibilities"])
                 , ("around"    , ["Area","Location"])
                 , ("as"        , ["Role"])
                 , ("astride"   , ["Location"])
                 , ("at"        , ["Location","Time"])
                 , ("atop"      , ["Location"])
                 -- ontop
                 -- bar
                 , ("before"    , ["Time"])
                 , ("behind"    , ["Location"])
                 , ("below"     , ["Location"])
                 , ("beneath"   , ["Location"])
                 , ("beside"    , ["Location"])
                 , ("besides"   , ["Location"])
                 , ("between"   , ["Area","Location"])
                 , ("beyond"    , ["Standard_item","Domain"])
                 -- but
                 , ("by"        , ["Means","Location"])
                 -- circa
                 -- come
                 , ("despite"   , ["Event_description"])
                 , ("down"      , ["Direction"])
                 , ("during"    , ["Duration"])
                 -- except
                 , ("for"       , ["Purpose","Duration","Goal"])
                 , ("from"      , ["Origin","Support"])
                 , ("in"        , ["Location","Time"])
                 , ("inside"    , ["Area","Location","Ground"])
                 , ("into"      , ["Goal","Effect"])
                 -- less
                 , ("like"      , ["Manner"])
                 -- minus
                 , ("near"      , ["Location"])
                 -- notwithstanding
                 , ("of"        , ["Phenomenon"])
                 , ("off"       , ["Location"])
                 , ("on"        , ["Location","Time"])
                 -- onto
                 -- opposite
                 , ("out"       , ["Direction"])
                 , ("outside"   , ["Area","Ground"])
                 , ("over"      , ["Duration","Location","Area","Ground"])
                 , ("past"      , ["Path","Area","Ground","Ground"])
                 -- per
                 -- save
                 -- short
                 , ("since"     , ["Time"])
                 -- than
                 , ("through"   , ["Path","Duration","Ground"])
                 , ("througout" , ["Path","Duration","Ground"])
                 , ("to"        , ["Goal","Effect","Direction"])
                 , ("toward"    , ["Goal","Direction"])
                 , ("towards"   , ["Goal","Direction"])
                 , ("under"     , ["Location","Ground","Area"])
                 , ("underneath", ["Location"])
                 , ("unlike"    , ["Manner"])
                 , ("until"     , ["Time"])
                 , ("till"      , ["Time"])
                 , ("up"        , ["Direction"])
                 , ("upon"      , ["Location"])
                 -- upside
                 -- versus
                 -- via
                 , ("with"      , ["Co-participant"])
                 , ("within"    , ["Location","Ground","Area"])
                 , ("without"   , ["Co-participant"])
                 -- worth
                 ]


mkTriples :: SentStructure -> ([X'Tree '[Lemma]],[(VerbStructure, CP '[Lemma])])
mkTriples sstr =
  let x'tr = sstr^.ss_x'tr
  in ( x'tr
     , [(vstr,cp)| vstr <- sstr ^.ss_verbStructures
                 , let vp = vstr^.vs_vp
                 , cp <- maybeToList $ do
                           let tagged = sstr^.ss_tagged
                           cp0 <- (^._1) <$> constructCP tagged vp
                           let rng_cp0 = cp0^.maximalProjection
                           (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng_cp0)) x'tr)
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
             -> Maybe (SpecTP '[Lemma])
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, CompVP '[Lemma])
matchSubject rolemap mDP patt = do
  dp <- mDP^?_Just._SpecTP_DP            -- for the time being
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
        pps_adj = cp^..complement.complement.adjunct.traverse._AdjunctVP_PP
        pps_comp = do
          compvp <- compvps
          case compvp of
            CompVP_PP pp ->
              let pp's = pp^..complement._CompPP_DP.adjunct.traverse._AdjunctDP_PP
              in (pp:pp's)
            CompVP_DP dp -> dp^..adjunct.traverse._AdjunctDP_PP
            _            -> []
    find ppcheck (pps_comp ++ pps_adj)
  where
    ppcheck pp = let HeadPP prep' pclass' = pp^.headX
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
        -> (Maybe (SpecTP '[Lemma]), VerbP '[Lemma], CP '[Lemma])
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, CompVP '[Lemma])])
matchSO rolemap (mDP,verbp,cp) (patt,num) =
  let rmatched0 = case verbp^.headX.vp_voice of
                    Active  -> maybeToList (matchSubject rolemap mDP patt) ++ matchObjects rolemap verbp patt
                    Passive -> maybeToList (matchAgentForPassive rolemap cp patt) ++ matchObjects rolemap verbp patt
      rmatched1 = rmatched0 ++ maybeToList (matchExtraRolesForPPTime cp rmatched0)
  in ((patt,num),rmatched1 ++ matchPrepArgs rolemap cp patt rmatched1)


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
           -> Maybe (SpecTP '[Lemma])
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
                            -> Maybe (SpecTP '[Lemma])
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
                   -> Maybe (SpecTP '[Lemma])
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
  guard (is _Nothing (find (\x -> x^._1 == "Time" || x^._1 == "Duration") felst))
  pp <- matchPP cp (Nothing,Just PC_Time,Just False)
  let comp = CompVP_PP pp
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  ((do prep <- pp^?headX.hp_prep._Prep_WORD
       find (\x -> (x^._1 == prep) && ("Duration" `elem` x^._2)) ppExtraRoleMap
       return ("Duration",comp)
    )
   <|> return ("Time",comp))



matchExtraRolesForGenericPP :: [Text]
                            -> CP '[Lemma]
                            -> [(FNFrameElement, CompVP '[Lemma])]
                            -> Maybe (FNFrameElement,CompVP '[Lemma])
matchExtraRolesForGenericPP fes cp felst = do
  let fes0 = map (^._1) felst
  pp <- matchPP cp (Nothing,Just PC_Other,Just False)
  prep <- pp^?headX.hp_prep._Prep_WORD
  let roles = ppExtraRoles prep
  role <- find (\r -> r /= "Time" && r /= "Duration" && unFNFrameElement r `elem` fes && not (r `elem` fes0)) roles
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just (pp^.complement.to compPPToRange)) felst))
  let comp = CompVP_PP pp
  return (role,comp)


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
  let rng_cp = cp^.maximalProjection
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng_cp) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.maximalProjection == Just rng_cp) felst))
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
  let rng_cp = cp^.maximalProjection
  guard (is _Nothing (find (\x -> x^?_2._CompVP_PP.complement.to compPPToRange == Just rng_cp) felst))
  guard (is _Nothing (find (\x -> x^?_2._CompVP_CP.maximalProjection == Just rng_cp) felst))
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
matchExtraRoles :: FrameDB
                -> FNFrame
                -> CP '[Lemma]
                -> [(FNFrameElement, CompVP '[Lemma])]
                -> [(FNFrameElement, CompVP '[Lemma])]
matchExtraRoles frmdb frame cp felst =
  let mmeans = matchExtraRolesForPPing "by" "Means" cp felst
      felst1 = felst ++ maybeToList mmeans
      mcomp  = matchExtraRolesForCPInCompVP toInfinitive "Purpose" cp felst1 <|>
               matchExtraRolesForPPing "after"  "Time_vector" cp felst1 <|>
               matchExtraRolesForPPing "before" "Time_vector" cp felst1
      felst2 = felst1 ++ maybeToList mcomp
      madj   = matchExtraRolesForCPInAdjunctCP (hasComplementizer ["as"]) "Explanation" cp felst2 <|>
               matchExtraRolesForCPInAdjunctCP toInfinitive               "Purpose"     cp felst2 <|>
               matchExtraRolesForCPInAdjunctCP (not.hasComplementizer ["after","before","as","while","if","though","although","unless"]) "Event_description" cp felst2 --for the time being
      felst3 = felst2 ++ maybeToList madj
      fes = do frm <- maybeToList (HM.lookup (unFNFrame frame) (frmdb^.frameDB))
               frm^..frame_FE.traverse.fe_name
      madjpp = matchExtraRolesForGenericPP fes cp felst3
  in felst3 ++ maybeToList madjpp


matchSubFrame :: (CP '[Lemma] -> Bool)
              -> Text
              -> (FNFrame,FNFrameElement,FNFrameElement)
              -> CP '[Lemma]
              -> Maybe (FNFrame,Text,[(FNFrameElement,(Bool,Range))])
matchSubFrame check prep (frm,fe0,fe1) cp0 = do
  let rng0 = cp0^.maximalProjection
  let candidates = cp0 ^..complement.complement.complement.traverse.trResolved._Just._CompVP_CP ++ cp0^..adjunct.traverse._AdjunctCP_CP
  cp1 <- find check candidates
  let rng1 = cp1^.maximalProjection
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



matchFrame :: FrameDB
           -> (VerbStructure,CP '[Lemma])
           -> Maybe (Range,VerbProperty (Zipper '[Lemma]),FrameMatchResult,Maybe (SenseID,Bool))
matchFrame frmdb (vstr,cp) =
    if verbp^.headX.vp_lemma == "be"
    then do
      dp_subj <- mDP^?_Just._SpecTP_DP   -- for the time being, ignore CP subject
      c <- listToMaybe (verbp^..complement.traverse.trResolved._Just)
      ((do dp_obj <- c^?_CompVP_DP
           let argpatt = ArgPattern Nothing (Just (GR_NP (Just GASBJ))) (Just (GR_NP (Just GA1))) Nothing Nothing Nothing
               role_subj = (FNFrameElement "Instance",CompVP_DP dp_subj)
               role_obj  = (FNFrameElement "Type",CompVP_DP dp_obj)
           return (rng_cp,vprop,FMR "Instance" (Just ((argpatt,1),[role_subj,role_obj])) [],Nothing))
       <|>
       (do pp_obj <- c^?_CompVP_PP
           prep_obj <- pp_obj^?headX.hp_prep._Prep_WORD
           (frm,rsbj,robj) <- ppRelFrame prep_obj
           let argpatt = ArgPattern Nothing (Just (GR_NP (Just GASBJ))) (Just (GR_PP Nothing)) Nothing Nothing Nothing
               role_subj = (rsbj,CompVP_DP dp_subj)
               role_obj  = (robj,CompVP_PP pp_obj)
           return (rng_cp,vprop,FMR frm (Just ((argpatt,1),[role_subj,role_obj])) [],Nothing)))


    else do
      ((frame,sense,mselected0),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)
      let mselected1 = (_Just . _2 %~ matchExtraRoles frmdb frame cp) mselected0
          mselected  = (_Just . _2 %~ resolveAmbiguityInDP) mselected1
          subfrms = mapMaybe (\(chk,prep,frm) -> matchSubFrame chk prep frm cp)
                      [(hasComplementizer ["after"] , "after" , ("Time_vector","Event","Landmark_event"))
                      ,(hasComplementizer ["before"], "before", ("Time_vector","Event","Landmark_event"))
                      ,(hasComplementizer ["while"] , "while" , ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                      ,(hasComplementizer ["though","although"], "though", ("Concessive","Main_assertion","Conceded_state_of_affairs"))
                      ,(hasComplementizer ["if"]    , "if"    , ("Conditional_occurrence","Consequence","Profiled_possibility"))
                      ,(hasComplementizer ["unless"], "unless", ("Negative_conditional","Anti_consequence","Profiled_possibility"))
                      ]
      return (rng_cp,vprop,FMR frame mselected subfrms,Just sense)
  where
    verbp = cp^.complement.complement
    mDP = cp^.complement.specifier.trResolved
    vprop = vstr^.vs_vp
    rng_cp = cp^.maximalProjection
    frmsels = matchFrameRolesAll verbp cp mDP (vstr^.vs_roleTopPatts)
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
              -> [X'Tree '[Lemma]]
              -> TaggedLemma '[Lemma]
              -> DetP '[Lemma]
              -> Maybe (Lemma,Lemma,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))
matchNomFrame apredata x'tr tagged dp = do
    (b,e) <- dp^?complement._Just.headX.hn_range
    -- for the time being, treat only single word nominal as frame
    guard (b==e)
    -- For the time being, I identify nominal frame only for a noun phrase with of, or with to-infinitive.
    let rng_dp = dp^.maximalProjection
        wndb = apredata^.analyze_wordnet
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
          return (EI rng rng Nothing txt)
    (verb,_senses,rmtoppatts) <- getFirst . mconcat $ do
      verb <- extractNominalizedVerb wndb lma
      let (senses,rmtoppatts) = getVerbSenses apredata verb
      guard ((not.null) senses && (not.null) rmtoppatts)
      (return . First . Just) (verb,senses,rmtoppatts)
    (((_sid,rolemap),_),patts) <- listToMaybe rmtoppatts
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
      pp <- dp^?complement._Just.complement._Just._CompDP_PP
      guard (pp^.headX.hp_prep == Prep_WORD "of")
      rng_obj <- pp^?complement._CompPP_DP.maximalProjection
      let txt_obj = T.intercalate " " (tokensByRange tagged rng_obj)
      return (lma,verb,(FNFrame frm,rng_dp),(subj,mei_subj),(obj,(EI rng_obj rng_obj (Just "of") txt_obj)))
    cpcase patt rolemap lma frm verb rng_dp mei_subj = do
      let (ms,mo) = subjObjSBAR (patt^._1)
      (args,_) <- ms
      (argo,_) <- mo
      subj <- FNFrameElement <$> lookup args rolemap
      obj  <- FNFrameElement <$> lookup argo rolemap
      compdp <- dp^?complement._Just.complement._Just
      cp_obj <- case compdp of
                   CompDP_Unresolved rng_obj -> (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng_obj)) x'tr)
                   CompDP_CP cp_obj -> Just cp_obj
                   _ -> Nothing
      let vp = cp_obj^.complement.complement
          rng_obj = cp_obj^.maximalProjection
          txt_obj = T.intercalate " " (tokensByRange tagged rng_obj)

      aux <- listToMaybe (vp^..headX.vp_auxiliary.traverse._2._2.to unLemma)
      guard (aux == "to")
      return (lma,verb,(FNFrame frm,rng_dp),(subj,mei_subj),(obj,(EI rng_obj rng_obj Nothing txt_obj)))
