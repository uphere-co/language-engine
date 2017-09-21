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
import           Control.Lens
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
import           NLP.Syntax.Format            (formatDP)
import           NLP.Syntax.Noun              (splitPP)
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty      (Voice(..))
import           NLP.Type.TagPos              (TagPos,TokIdx)
--
import           SRL.Analyze.Parameter        (roleMatchWeightFactor)
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,AdjustedDetP(..), _WithPrep, getDetP
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



formatAdjustedDetP (WithPrep p _ z)  = "prep:"  <> p <> ", " <> formatDP z
formatAdjustedDetP (WithoutPrep z) = formatDP z



mkPAWSTriples :: SentStructure
              -> ([Bitree (Range, CPDP '[Lemma]) (Range, CPDP '[Lemma])]
                 ,[(VerbStructure, PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))])
mkPAWSTriples sstr =
  let clausetr = sstr^.ss_clausetr
      cpstr = sstr^.ss_cpstr
  in ( cpstr
     , [(vstr,paws)| vstr <- sstr ^.ss_verbStructures
                   , let vp = vstr^.vs_vp
                   , paws <- maybeToList (findPAWS (sstr^.ss_tagged) clausetr vp cpstr) ]
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
             -> DetP '[Lemma]
             -> ArgPattern p GRel
             -> Maybe (FNFrameElement, AdjustedDetP)
matchSubject rolemap dp patt = do
  (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
  (,WithoutPrep dp) <$> lookup p rolemap


matchObjects :: [(PBArg,FNFrameElement)]
             -> VerbP '[Lemma]
             -> ArgPattern p GRel
             -> [(FNFrameElement, AdjustedDetP)]
matchObjects rolemap verbp patt = do
  (garg,obj') <- zip [GA1,GA2] (verbp^..complement.traverse.trResolved.to (\x -> x >>= \case DP z -> Just z; _ -> Nothing))
  obj <- maybeToList obj'
  ctag <- case obj ^. maximalProjection.to current.to getRoot of
            Left (_,node) -> [chunkTag node]
            _             -> []
  (p,a) <- maybeToList (pbArgForGArg garg patt)
  case ctag of
    NP   -> guard (a == GR_NP   (Just garg))
    S    -> guard (a == GR_SBAR (Just garg))
    SBAR -> guard (a == GR_SBAR (Just garg))
    _    -> []
  fe <- maybeToList (lookup p rolemap)
  return (fe, WithoutPrep obj)



matchPP :: [TagPos TokIdx MarkType]
        -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
        -> (Text,Maybe Bool)
        -> Maybe (Zipper '[Lemma],DetP '[Lemma])
matchPP tagged paws (prep,mising) = do
    Left (rng,_) <- find ppcheck (paws^.pa_candidate_args)
    tr <- current . root <$> paws^.pa_CP.maximalProjection
    z' <- (find (\z -> z^?to current._PN._1._1 == Just rng) . getNodes .mkBitreeZipper []) tr
    return (z',splitPP tagged z')
  where
    ppcheck (Left (_,S_PP prep' ising')) = prep == prep' && maybe True (\ising -> ising == ising') mising
    ppcheck _                            = False


matchPrepArgs :: [(PBArg,FNFrameElement)]
              -> [TagPos TokIdx MarkType]
              -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
              -> ArgPattern p GRel
              -> [(FNFrameElement, AdjustedDetP)]
matchPrepArgs rolemap tagged paws patt = do
  (p,(prep,mising)) <- pbArgForPP patt
  (o,z) <- maybeToList (matchPP tagged paws (prep,mising))
  (,WithPrep prep o z) <$> maybeToList (lookup p rolemap)


matchAgentForPassive :: [(PBArg,FNFrameElement)]
                     -> [TagPos TokIdx MarkType]
                     -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, AdjustedDetP)
matchAgentForPassive rolemap tagged paws patt = do
    (p,GR_NP (Just GASBJ)) <- pbArgForGArg GASBJ patt
    (o,z) <- matchPP tagged paws ("by",Nothing)
    (,WithPrep "by" o z) <$> lookup p rolemap



matchThemeForPassive :: [(PBArg,FNFrameElement)]
                     -> DetP '[Lemma]
                     -> ArgPattern p GRel
                     -> Maybe (FNFrameElement, AdjustedDetP)
matchThemeForPassive rolemap dp patt = do
  (p,GR_NP (Just GA1)) <- pbArgForGArg GA1 patt
  (,WithoutPrep dp) <$> lookup p rolemap


matchSO :: [(PBArg,FNFrameElement)]
        -> [TagPos TokIdx MarkType]
        -> (DetP '[Lemma], VerbP '[Lemma], PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag)))
        -> (ArgPattern p GRel, Int)
        -> ((ArgPattern p GRel, Int), [(FNFrameElement, AdjustedDetP)])
matchSO rolemap tagged (dp,verbp,paws) (patt,num) =
  case verbp^.headX.vp_voice of
    Active -> ((patt,num), maybeToList (matchSubject rolemap dp patt) ++ matchObjects rolemap verbp patt ++ matchPrepArgs rolemap tagged paws patt )
    Passive -> ((patt,num),catMaybes [matchAgentForPassive rolemap tagged paws patt,matchThemeForPassive rolemap dp patt] ++ matchPrepArgs rolemap tagged paws patt)


extendRoleMapForDual :: Text -> [(PBArg,FNFrameElement)] -> (Text, [(PBArg,FNFrameElement)])
extendRoleMapForDual frame rolemap = fromMaybe (frame,rolemap) $ do
  dualmap <- lookup frame $ map (\c -> (c^.cm_baseFrame,c)) causeDualMap
  let frame' = dualmap^.cm_causativeFrame
      rolemap' = filter (\(k,_v) -> k /= "frame" && k /= "arg0") rolemap
      rolemap'' =  map f rolemap'
        where f (k,v) = maybe (k,v) (k,) (lookup v (dualmap^.cm_extraMapping))
      rolemap''' = ("arg0",dualmap^.cm_externalAgent) : rolemap''
  return (frame',rolemap''')


numMatchedRoles :: ((ArgPattern () GRel, Int), [(FNFrameElement, a)]) -> Int
numMatchedRoles = lengthOf (_2.folded)


matchRoles :: [(PBArg,FNFrameElement)]
           -> [TagPos TokIdx MarkType]
           -> VerbP '[Lemma]
           -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
           -> [(ArgPattern () GRel, Int)]
           -> DetP '[Lemma]
           -> Maybe ((ArgPattern () GRel, Int),[(FNFrameElement, AdjustedDetP)])
matchRoles rolemap tagged verbp paws toppattstats dp =
    (listToMaybe . sortBy cmpstat . head . groupBy eq . sortBy (flip compare `on` numMatchedRoles)) matched
  where
    matched = map (matchSO rolemap tagged (dp,verbp,paws)) toppattstats
    cmpstat  = flip compare `on` (^._1._2)
    eq       = (==) `on` lengthOf (_2.folded)



matchFrameRolesForCauseDual :: [TagPos TokIdx MarkType]
                            -> VerbP '[Lemma]
                            -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
                            -> [(ArgPattern () GRel,Int)]
                            -> Maybe (DetP '[Lemma])
                            -> LittleV
                            -> (Text, [(PBArg, FNFrameElement)])
                            -> (Text, Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, AdjustedDetP)]))
matchFrameRolesForCauseDual tagged verbp paws toppatts mDP causetype (frame1,rolemap1) =
  let (frame2,rolemap2) = if causetype == LVDual
                          then extendRoleMapForDual frame1 rolemap1
                          else (frame1,rolemap1)
      mselected1 = join (matchRoles rolemap1 tagged verbp paws toppatts <$> mDP)
      mselected2 = join (matchRoles rolemap2 tagged verbp paws toppatts <$> mDP)
  in case (mselected1,mselected2) of
       (Nothing,Nothing) -> (frame1,Nothing)
       (Just _ ,Nothing) -> (frame1,mselected1)
       (Nothing,Just _ ) -> (frame2,mselected2)
       (Just s1,Just s2) ->
         case (compare `on` numMatchedRoles) s1 s2 of
           GT -> (frame1,mselected1)
           LT -> (frame2,mselected2)
           EQ -> (frame1,mselected1)   -- choose intransitive because transitive should
                                       -- have one more argument in general.


matchFrameRolesAll :: [TagPos TokIdx MarkType]
                   -> VerbP '[Lemma]
                   -> PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))
                   -> Maybe (DetP '[Lemma])
                   -> [((RoleInstance,Int),[(ArgPattern () GRel,Int)])]
                   -> [((Text,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement,AdjustedDetP)])),Int)]
matchFrameRolesAll tagged verbp paws mDP rmtoppatts = do
  (rm,toppatts) <- rmtoppatts
  let rolemap1 = rm^._1._2
      stat = rm^._2
  frame1 <- maybeToList (lookup "frame" rolemap1)
  causetype <- (\x -> if x == "dual" then LVDual else LVSingle) <$> maybeToList (lookup "cause" rolemap1)
  return (matchFrameRolesForCauseDual tagged verbp paws toppatts mDP causetype (frame1,rolemap1),stat)


-- | this function should be generalized.
--
matchExtraRoles :: [TagPos TokIdx MarkType]
                -> PredArgWorkspace '[Lemma] (Either (Range, STag) (Int, POSTag))
                -> [(FNFrameElement,AdjustedDetP)]
                -> [(FNFrameElement,AdjustedDetP)]
matchExtraRoles tagged paws felst =
  let mmeans = do
        guard (isNothing (find (\x -> x^._1 == "Means") felst))
        (o,z) <-matchPP tagged paws ("by",Just True)
        let rng = headRange z
        guard (isNothing (find (\x -> x^?_2._WithPrep._3.to headRange == Just rng) felst))
        return ("Means",WithPrep "by" o z)
  in maybe felst (\means -> means : felst) mmeans


-- | A scoring algorithm for selecting a frame among candidates.
--   This version is ad hoc, so it will be updated when we come up with a better algorithm.
--
scoreSelectedFrame :: Int
                   -> ((Text,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement,a)])),Int)
                   -> Double
scoreSelectedFrame total ((_,mselected),n) =
  let mn = maybe 0 fromIntegral (mselected^?_Just.to numMatchedRoles)
  in mn * (fromIntegral n) / (fromIntegral total) * roleMatchWeightFactor + (mn*(fromIntegral total))



-- | Resolve PP ambiguity for matched PP
--   This algorithm is ad hoc but practically works for PP embedded in DP by CoreNLP.
--   We need to have a systematic treatment and tests for PP ambiguity.
--
resolveAmbiguityInDP :: [(FNFrameElement, AdjustedDetP)]
                     -> [(FNFrameElement, AdjustedDetP)]
resolveAmbiguityInDP lst = foldr1 (.) (map go lst) lst
  where
    go :: (FNFrameElement,AdjustedDetP)
       -> [(FNFrameElement,AdjustedDetP)]
       -> [(FNFrameElement,AdjustedDetP)]
    go (fe,WithPrep _ o z) lst = map (f (fe,getRange (current o))) lst
    go (fe,WithoutPrep z) lst = map (f (fe,z^.headX._2)) lst

    f (fe,rng@(b,e)) (fe',WithPrep prep' o' z')
      = let rng'@(b',e') = z'^.headX._1
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let z'' = ((headX .~ ((b,e),(b',b-1))) . (adjunct .~ Nothing {- (Just (b,e)) -} )) z'
               in (fe', WithPrep prep' o' z'')
          else (fe',WithPrep prep' o' z')
    f (fe,rng@(b,e)) (fe',WithoutPrep z')
      = let rng'@(b',e') = z'^.headX._1
        in -- for the time being, use this ad hoc algorithm
          if fe /= fe' && rng `isInsideR` rng' && b /= b'
          then let z'' = ((headX .~ ((b,e),(b',b-1))) . (adjunct .~ Nothing {- (Just (b,e)) -} )) z'
               in (fe', WithoutPrep z'')
          else (fe',WithoutPrep z')



matchFrame :: [TagPos TokIdx MarkType]
           -> (VerbStructure,PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag)))
           -> Maybe (Range,VerbProperty (Zipper '[Lemma]),Text
                    ,Maybe ((ArgPattern () GRel,Int),[(FNFrameElement, AdjustedDetP)]))
matchFrame tagged (vstr,paws) = do
  let cp = paws^.pa_CP
      verbp = cp^.complement.complement
      mDP = cp^.complement.specifier.trResolved
      vprop = vstr^.vs_vp
  rng <- cpRange cp
  let frmsels = matchFrameRolesAll tagged verbp paws mDP (vstr^.vs_roleTopPatts)
      total=  sum (frmsels^..traverse._2)
  ((frame,mselected0),_) <- listToMaybe (sortBy (flip compare `on` scoreSelectedFrame total) frmsels)
  let mselected1 = (_Just . _2 %~ matchExtraRoles tagged paws) mselected0
      mselected  = (_Just . _2 %~ resolveAmbiguityInDP) mselected1
  return (rng,vprop,frame,mselected)




depCPDP :: Bitree (Range,a) (Range,a) -> [(Range,Range)]
depCPDP (PN (rng0,_) xs) = map ((rng0,) . fst . getRoot1) xs ++ concatMap depCPDP xs
depCPDP (PL _)           = []


meaningGraph :: SentStructure -> MeaningGraph
meaningGraph sstr =
  let (cpstr,lst_vstrpaws) = mkPAWSTriples sstr
      matched = mapMaybe (matchFrame (sstr^.ss_tagged)) lst_vstrpaws
      depmap = depCPDP =<< cpstr
      --
      preds = flip map matched $ \(rng,vprop,frame,_mselected) i -> MGPredicate i rng frame (simplifyVProp vprop)
      ipreds = zipWith ($) preds [1..]
      --
      entities0 = do (_,_,_,mselected) <- matched
                     (_,felst) <- maybeToList mselected
                     z <- (^._2 . to getDetP) <$> felst
                     let rng = headRange z
                         mrngtxt' = do rng <- case (z^.adjunct, z^.complement) of
                                                (Just r,_) -> return r -- for the time being
                                                _          -> Nothing
                                       let txt = z ^.  maximalProjection
                                                      .to current
                                                      .to (tokensByRange rng)
                                                      .to (T.intercalate " ")
                                       return (rng,txt)
                         txt = headText z
                     return (rng,txt,mrngtxt')

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
      rangeid (MGPredicate _ rng _ _)      = (0,rng)
      rangeid (MGNominalPredicate _ rng _) = (1,rng)
      rngidxmap = HM.fromList [(rangeid v, v^.mv_id) | v <- vertices ]
      edges0 = do (rng,_,_,mselected) <- matched
                  i <- maybeToList (HM.lookup (0,rng) rngidxmap)   -- frame
                  (_,felst) <- maybeToList mselected
                  (fe,x) <- felst
                  let z = getDetP x
                      mprep = x ^? _WithPrep . _1
                  let rng' = headRange z
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
