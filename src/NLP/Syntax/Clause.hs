{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Lens.Extras                    (is)
import           Control.Monad                          ((<=<),(>=>),guard,void)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,execState,get,put)
import           Data.Bifoldable
import           Data.Bitraversable                     (bitraverse)
import           Data.Either                            (partitionEithers,rights)
import qualified Data.HashMap.Strict               as HM
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (First(..),Last(..))
import           Data.Text                              (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           Data.List                              (find)
import           Data.ListZipper
import           Data.Range                             (rangeTree)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Voice(..))
--
import           NLP.Syntax.Noun                        (splitDP,mkPPFromZipper)
import           NLP.Syntax.Preposition                 (checkEmptyPrep,checkTimePrep,isMatchedTime
                                                        ,identifyInternalTimePrep)
import           NLP.Syntax.Type                        (ClauseTree,ClauseTreeZipper,SBARType(..),STag(..),PredArgWorkspace(..))
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs,isPOSAs,mergeLeftELZ,mergeRightELZ,rootTag)
--
import Debug.Trace
import qualified Data.Text as T
import NLP.Syntax.Format.Internal

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return


maximalProjectionVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
maximalProjectionVP vp = listToMaybe (vp^.vp_words) >>= parent . fst


parentOfVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
parentOfVP vp = parent =<< maximalProjectionVP vp


headVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
headVP vp = getLast (mconcat (map (Last . Just . fst) (vp^.vp_words)))


complementCandidates :: VerbProperty (Zipper (Lemma ': as)) -> Zipper (Lemma ': as) -> [Zipper (Lemma ': as)]
complementCandidates vprop z_vp =
    let cs0 = siblingsBy (nextNotComma) checkNPSBAR =<< maybeToList (headVP vprop)
        cs1 = maybeToList $ do
                guard (isChunkAs VP (current z_vp))
                z_np <- prev z_vp
                guard (isChunkAs NP (current z_np))
                z_comma <- prev z_np
                guard (isPOSAs M_COMMA (current z_comma))
                z_cp <- prev z_comma
                (guard (isChunkAs S (current z_cp) || isChunkAs SBAR (current z_cp)))
                return z_cp
    in cs0 ++ cs1

  where
    nextNotComma z = do n <- next z
                        guard (not (isPOSAs M_COMMA (current n))) -- ad hoc separation using comma
                        return n
    checkNPSBAR z = case rootTag z of
                      Left NP    -> True
                      Left PP    -> True
                      Left SBAR  -> True
                      Left S     -> True
                      Left SBARQ -> True
                      Left SQ    -> True
                      Left _     -> False
                      Right p    -> isNoun p == Yes


complementsOfVerb :: TaggedLemma (Lemma ': as)
                  -> VerbProperty (Zipper (Lemma ': as))
                  -> Zipper (Lemma ': as)
                  -> ([TraceChain (CompVP (Lemma ': as))],[Zipper (Lemma ': as)])
complementsOfVerb tagged vprop z_vp =
  let (cs,zs) = let lst = map xform (complementCandidates vprop z_vp)
                in (map fst lst,concatMap snd lst)    
  in case vprop^.vp_voice of
       Active -> (cs,zs)
       Passive -> (TraceChain (Left (singletonLZ Moved)) Nothing : cs,zs)
  where
    xform_dp z = let dp = splitDP tagged (mkOrdDP z)
                     (dp',zs) = identifyInternalTimePrep tagged dp
                 in (TraceChain (Right []) (Just (checkEmptyPrep tagged dp')), zs)
    xform_pp z = (TraceChain (Right []) (checkTimePrep tagged <$> mkPPFromZipper tagged PC_Other z), [])
      -- checkTimePrep tagged
                    
    xform_cp z = (TraceChain (Right []) (Just (CompVP_Unresolved z)), [])
    xform z = case rootTag (current z) of
                Left NP    -> xform_dp z
                Left PP    -> xform_pp z
                Left _     -> xform_cp z
                Right p    -> if isNoun p == Yes then xform_dp z else xform_cp z




allAdjunctCPOfVerb :: VerbProperty (Zipper (Lemma ': as))
                   -> [AdjunctCP (Lemma ': as)]
allAdjunctCPOfVerb vprop =
    let mcomma = firstSiblingBy next (isPOSAs M_COMMA) =<< headVP vprop  -- ad hoc separation using comma
    in case mcomma of
         Nothing -> []
         Just comma -> map AdjunctCP_Unresolved (siblingsBy next checkS comma)
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkS z = case tag z of
                    Left SBAR  -> True
                    Left S     -> True
                    Left SBARQ -> True
                    Left SQ    -> True
                    _          -> False



identifySubject :: TaggedLemma (Lemma ': as)
                -> N.ClauseTag
                -> Zipper (Lemma ': as)   -- ^ Verb maximal projection
                -> TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as)))
identifySubject tagged tag vp = maybe nul smp r
  where
    r = case tag of
          N.SINV -> firstSiblingBy next (isChunkAs NP) vp          -- this should be refined.
          _      -> firstSiblingBy prev (isChunkAs NP) vp          -- this should be refined.
    nul = TraceChain (Left (singletonLZ NULL)) Nothing
    smp z = TraceChain (Right [])(Just (Right (splitDP tagged (mkOrdDP z)))) -- for the time being, CP subject is not supported


--
-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: TaggedLemma (Lemma ': as)
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as),[Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))])
constructCP tagged vprop = do
    z_vp <- maximalProjectionVP vprop
    tp <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp
    case tptag' of
      N.CL s -> do
        cp' <- parent tp
        cptag' <- N.convert <$> getchunk cp'
        let (comps,cadjs) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToEither)
            subj0 = identifySubject tagged s z_vp
            (subj,subj_dps,sadjs) = fromMaybe (subj0,[],[]) $ do
              dp_subj <- subj0 ^? trResolved . _Just . _Right
              let (dp_subj',sadjs') = identifyInternalTimePrep tagged dp_subj
                  dp_subj'' = splitDP tagged dp_subj' 
                  subj' = ((trResolved . _Just . _Right) .~ dp_subj'') subj0
              subj_dp <- subj' ^? trResolved . _Just
              return (subj',[subj_dp],sadjs')
            verbp = mkVerbP z_vp vprop (cadjs++sadjs) comps
            dps = subj_dps ++ comps_dps
        case cptag' of
          N.CL N.SBAR ->
            let (cphead,cpspec) = case prev tp of
                                    Nothing -> (C_PHI,Nothing)
                                    Just z -> if (isChunkAs WHNP (current z))
                                              then (C_PHI,Just (SpecCP_WH z))
                                              else (C_WORD z,Nothing)
            in return (mkCP cphead cp' cpspec adjs (mkTP tp subj verbp),dps)
          N.CL _ ->
            return (mkCP C_PHI tp Nothing adjs (mkTP tp subj verbp),dps)
          N.RT   ->
            return (mkCP C_PHI cp' Nothing adjs (mkTP tp subj verbp),dps)
          _      -> -- somewhat problematic case?
            return (mkCP C_PHI tp Nothing adjs (mkTP tp subj verbp),dps)
      _ -> -- reduced relative clause
        let (comps,cadjs) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToEither)
            verbp = mkVerbP z_vp vprop cadjs comps
            nullsubj = TraceChain (Left (singletonLZ NULL)) Nothing
        in return (mkCP C_PHI z_vp (Just SpecCP_WHPHI) adjs (mkTP z_vp nullsubj verbp),comps_dps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



cpRange :: CP xs -> Range
cpRange cp = cp^.maximalProjection.to (getRange . current)


hierarchyBits :: TaggedLemma t -> (CP t, [DetP t]) -> Maybe [(Range, (Range, CPDPPP t))]
hierarchyBits tagged (cp,dps) = do
  let rng = cpRange cp
      cpbit = (rng,(rng,CPCase cp))
      f dp = let rng' = dp^.maximalProjection
                 dpbit = (rng',(rng',DPCase dp))
                 lst = do adj <- dp^.adjunct
                          case adj of
                            AdjunctDP_PP pp ->
                              let rng_pp = pp^.maximalProjection
                              in return (rng_pp,(rng_pp,PPCase pp))
                            AdjunctDP_Unresolved rng_pp -> maybeToList $ do
                              z_pp <- extractZipperByRange rng_pp (tagged^.pennTree)
                              (rng_pp,) . (rng_pp,) . PPCase <$> mkPPFromZipper tagged PC_Other z_pp
             in dpbit : lst
  return (cpbit:concatMap f dps)



identifyCPHierarchy :: TaggedLemma (Lemma ': as)
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> [X'Tree (Lemma ': as)]
identifyCPHierarchy tagged vps = fromMaybe [] (traverse (bitraverse tofull tofull) rtr)
  where x'map = (HM.fromList . concat . mapMaybe (hierarchyBits tagged . (_2 %~ rights) <=< constructCP tagged)) vps
        rngs = HM.keys x'map
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng x'map


currentCPDPPP :: X'Zipper as -> CPDPPP as
currentCPDPPP = snd . getRoot1 . current


--
-- | rewrite X'Tree. Now it's focusing on modifier relation, but this should
--   be more generalized.
--
rewriteX'TreeForModifier :: ((Range, CPDPPP as) -> (Range, CPDPPP as))
             -> X'Zipper as
             -> DetP as
             -> MaybeT (State (X'Tree as)) ()
rewriteX'TreeForModifier f w z = do
  let dprng = z^.maximalProjection
      -- rewrite X'Tree by modifier relation.
  case extractZipperById dprng (toBitree w) of
    Nothing -> do let newtr (PN y ys) = PN (dprng,DPCase z) [PN (f y) ys]
                      newtr (PL y)    = PN (dprng,DPCase z) [PL (f y)]
                      w'' = replaceFocusTree newtr w
                  lift (put (toBitree w''))
    Just _  -> do let otr = case current w of
                              PN y ys -> PN (f y) ys
                              PL y    -> PL (f y)
                  lift . put =<< hoistMaybe (removeFocusTree w)
                  w' <- MaybeT (extractZipperById dprng <$> get)
                  let newtr (PN y ys) = PN y (ys ++ [otr])
                      newtr (PL y)    = PN y [otr]
                      w'' =replaceFocusTree newtr w'
                  lift (put (toBitree w''))



retrieveWCP :: Range -> MaybeT (State (X'Tree t)) (X'Zipper t,CP t)
retrieveWCP rng = do
  tr <- lift get
  w <- hoistMaybe (extractZipperById rng tr)
  cp <- hoistMaybe (currentCPDPPP w ^? _CPCase)
  return (w,cp)


rewriteX'TreeForFreeWH :: Range -> [TraceType] -> X'Zipper t -> DetP t -> Maybe (X'Zipper t)
rewriteX'TreeForFreeWH rng ps w z' = do
  w_dom <- parent w
  cp_dom <- w_dom^?to currentCPDPPP._CPCase
  let vp_dom = cp_dom^.complement.complement
      comps = vp_dom^.complement
      comps' = flip map comps $ \comp -> fromMaybe comp $ do
                 z_comp <- comp^?trResolved._Just._CompVP_Unresolved
                 guard (getRange (current z_comp) == rng)
                 return (TraceChain (Right (ps++[Moved,WHPRO])) (Just (CompVP_DP z')))
      rf = _2._CPCase.complement.complement.complement .~ comps'
  return (replaceFocusItem rf rf w_dom)


whMovement :: TaggedLemma (Lemma ': as)
           -> (X'Zipper (Lemma ': as),CP (Lemma ': as))
           -> State (X'Tree (Lemma ': as))
                    (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
whMovement tagged (w,cp) = do
  -- letter z denotes zipper for PennTree, w denotes zipper for X'Tree
  let rng = cpRange cp
      spec = cp^.complement.specifier
  -- whMovement process starts with cheking trace in subject
  case spec^.trChain of
    Left (LZ ps c _ns) ->
      -- with trace in subject
      -- check subject for relative pronoun
      case c of
        NULL -> do
          -- ignore ns.
          let xspro = LZ ps SilentPRO []
          fmap (fromMaybe (TraceChain (Left xspro) Nothing)) . runMaybeT $ do
            -- check subject position for relative pronoun
            let z_cp = cp^.maximalProjection
            ((do -- ordinary relative clause
                 dp' <- splitDP tagged . mkOrdDP <$> hoistMaybe (firstSiblingBy prev (isChunkAs NP) z_cp)
                 rewriteX'TreeForModifier id w dp'
                 return (TraceChain (Left (LZ ps Moved [WHPRO])) (Just (Right dp'))))
             <|>
             (do -- free relative clause
                 dp' <- splitDP tagged . mkOrdDP <$> hoistMaybe (cp^?specifier._Just._SpecCP_WH)
                 w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng (reverse ps) w dp')
                 lift (put (toBitree w_dom'))
                 (w',_) <- retrieveWCP rng
                 rewriteX'TreeForModifier id w' dp'
                 return (TraceChain (Left (LZ ps Moved [WHPRO])) (Just (Right dp')))))
        _    -> return spec
    Right ps -> do
      -- without trace in subject
      -- check object for relative pronoun
      void . runMaybeT $ do
        let z_cp = cp^.maximalProjection
        ((do -- ordinary relative clause
             z' <- splitDP tagged . mkOrdDP <$> hoistMaybe (firstSiblingBy prev (isChunkAs NP) z_cp)
             let -- adjust function for complement with relative pronoun resolution
                 rf0 = _2._CPCase.complement.complement.complement
                         %~ (TraceChain (Left (LZ [] Moved [WHPRO])) (Just (CompVP_DP z')) :)
             -- rewrite X'Tree for modifier relation.
             rewriteX'TreeForModifier rf0 w z')
         <|>
         (do -- free relative clause
             z' <- splitDP tagged . mkOrdDP <$> hoistMaybe (cp^?specifier._Just._SpecCP_WH)
             w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng ps w z')
             lift (put (toBitree w_dom'))
             (w',_) <- retrieveWCP rng
             let -- adjust function for complement with relative pronoun resolution
                 rf0 = _2._CPCase.complement.complement.complement
                         %~ (TraceChain (Left (LZ [] Moved [WHPRO])) (Just (CompVP_DP z')) :)
             rewriteX'TreeForModifier rf0 w' z'))
      return spec


resolveSilentPRO :: TaggedLemma (Lemma ': as)
                 -> (X'Zipper (Lemma ': as),CP (Lemma ': as))
                 -> MaybeT (State (X'Tree (Lemma ': as))) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveSilentPRO tagged (z,cp) = do
  let spec = cp^.complement.specifier
  case spec^.trChain of
    Right _ -> return spec
    Left (xs@(LZ _ c _)) -> case c of
      NULL      -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                        let rng' = cpRange cp'
                        TraceChain exs' x' <- lift (resolveDP tagged rng')
                        return (TraceChain (mergeLeftELZ (Left (replaceLZ SilentPRO xs)) exs') x'))
                    <|>
                    return (TraceChain (Left (replaceLZ SilentPRO xs)) Nothing))
      SilentPRO -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                        let rng' = cpRange cp'
                        TraceChain exs' x' <- lift (resolveDP tagged rng')
                        -- let xs' = either lzToList id exs'
                        return (TraceChain (mergeLeftELZ (Left xs) exs') x'))
                    <|>
                    return (TraceChain (Left xs) Nothing))
      _         -> return spec

--
-- | resolve passive DP-movement. this is ad hoc yet.
--
resolveVPComp :: Range
              -> TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as)))
              -> MaybeT (State (X'Tree (Lemma ': as))) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveVPComp rng spec = do
  (w,cp) <- retrieveWCP rng
  let verbp = cp^.complement.complement
  case verbp^.headX.vp_voice of
    Active -> return spec
    Passive -> do
      let cs = verbp^.complement
      case cs of
        [] -> trace "No complements?" $ return spec
        c:rest -> do
          let r = either (const Nothing) (Just . CompVP_DP) =<< spec^.trResolved  -- ignore CP case for the time being.
              c' = TraceChain (mergeLeftELZ (c^.trChain) (spec^.trChain)) r
              rf = _2._CPCase.complement.complement.complement .~ (c':rest)
              w' = replaceFocusItem rf rf w
          lift (put (toBitree w'))
          return (TraceChain (mergeRightELZ (c^.trChain) (spec^.trChain)) (spec^.trResolved))


--
-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
resolveDP :: TaggedLemma (Lemma ': as)
          -> Range
          -> State (X'Tree (Lemma ': as)) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveDP tagged rng = fmap (fromMaybe emptyTraceChain) . runMaybeT $ do
  (w,cp) <- retrieveWCP rng
  if is _Just (cp^.specifier)  -- relative clause
    then resolveVPComp rng =<< lift (whMovement tagged (w,cp))
    else resolveVPComp rng =<< resolveSilentPRO tagged (w,cp)



resolveCP :: forall as. X'Tree (Lemma ': as) -> X'Tree (Lemma ': as)
resolveCP xtr = rewriteTree action xtr
  where
    action rng = do z <- hoistMaybe . extractZipperById rng =<< lift get
                    (replace z <|> return z)
    --
    replace :: X'Zipper (Lemma ': as) -> MaybeT (State (X'Tree (Lemma ': as))) (X'Zipper (Lemma ': as))
    replace = replaceCompVP >=> replaceAdjunctCP
    --
    replaceCompVP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let xs = cp^.complement.complement.complement
      xs' <- flip traverse xs $ \x ->
               ((do tr <- lift get
                    rng <- hoistMaybe (x^?trResolved._Just._CompVP_Unresolved.to current.to getRange)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (trResolved .~ Just (CompVP_CP cp'))) x
                )
                <|>
                (return x))
      let rf = _2._CPCase.complement.complement.complement .~ xs'
          z' = replaceFocusItem rf rf z
      lift (put (toBitree z'))
      return z'
    --
    replaceAdjunctCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let xs = cp^.adjunct
      xs' <- flip traverse xs $ \x ->
               ((do tr <- lift get
                    rng <- hoistMaybe (x^?_AdjunctCP_Unresolved.to current.to getRange)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    return (AdjunctCP_CP cp')
                )
                <|>
                (return x))
      let rf = _2._CPCase.adjunct .~ xs'
          z' = replaceFocusItem rf rf z
      lift (put (toBitree z'))
      return z'


bindingSpec :: Range -> TraceChain (Either (Zipper t) (DetP t)) -> MaybeT (State (X'Tree t)) (X'Zipper t)
bindingSpec rng spec = do
  z <- hoistMaybe . extractZipperById rng =<< lift get
  let rf = _2._CPCase.complement.specifier .~ spec
      z' = replaceFocusItem rf rf z
  lift (put (toBitree z'))
  return z'


-- consider passive case only now for the time being.
connectRaisedDP :: Range -> MaybeT (State (X'Tree (Lemma ': as))) (X'Zipper (Lemma ': as))
connectRaisedDP rng = do
  (w,cp) <- retrieveWCP rng
  guard (cp ^. complement.complement.headX.vp_voice == Passive)
  c1:c2:[] <- return (cp^.complement.complement.complement)
  rng1 <- hoistMaybe (c1^?trResolved._Just._CompVP_DP.headX)
  cp' <- hoistMaybe (c2^?trResolved._Just._CompVP_CP)
  rng_dp <- hoistMaybe (cp'^?complement.specifier.trResolved._Just._Right.headX)
  if rng1 == rng_dp
    then do
      let rf = (_2._CPCase.complement.specifier .~ emptyTraceChain)
             . (_2._CPCase.complement.complement.complement .~ [c2])
          w' = replaceFocusItem rf rf w
      lift (put (toBitree w'))
      return w'
    else
      return w


-- I think we should change the name of these bindingAnalysis.. functions.

--
-- | This is the final step to bind inter-clause trace chain
--
bindingAnalysis :: TaggedLemma (Lemma ': as) -> X'Tree (Lemma ': as) -> X'Tree (Lemma ': as)
bindingAnalysis tagged = rewriteTree $ \rng -> lift (resolveDP tagged rng) >>= bindingSpec rng


--
-- |
--
bindingAnalysisRaising :: X'Tree (Lemma ': as) -> X'Tree (Lemma ': as)
bindingAnalysisRaising = rewriteTree $ \rng -> do z <- hoistMaybe . extractZipperById rng =<< lift get
                                                  (connectRaisedDP rng <|> return z)



rewriteTree :: (Range -> MaybeT (State (X'Tree as)) (X'Zipper as))
            -> X'Tree as
            -> X'Tree as
rewriteTree action xtr = execState (go rng0) xtr
  where getrng = fst . getRoot1 . current
        rng0 = (either fst fst . getRoot) xtr
        go rng = void . runMaybeT $ do
                   z' <- action rng
                   ((hoistMaybe (child1 z') >>= \z'' -> lift (go (getrng z'')))
                    <|>
                    (hoistMaybe (next z') >>= \z'' -> lift (go (getrng z''))))



predicateArgWS :: TaggedLemma (Lemma ': as)
               -> CP (Lemma ': as)
               -> ClauseTreeZipper
               -> [Zipper (Lemma ': as)]
               -> PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag))
predicateArgWS tagged cp z adjs =
  PAWS { _pa_CP = cp
       , _pa_candidate_args = case child1 z of
                                Nothing -> []
                                Just z' -> map extractArg (z':iterateMaybe next z')
                              ++ let f x = flip fmap (mkPPFromZipper tagged PC_Time x) $ \pp ->
                                             let prep = fromMaybe "" (pp^?headX._1._Prep_WORD)
                                                 rng = pp ^. maximalProjection
                                             in Left (rng,S_PP prep PC_Time False)
                                 in mapMaybe f adjs

       }
  where extractArg x = case getRoot (current x) of
                         Left (rng,(stag,_))         -> Left  (rng,stag)
                         Right (Left (rng,(stag,_))) -> Left  (rng,stag)
                         Right (Right (i,(ptag,_)))  -> Right (i,ptag)
        iterateMaybe :: (a -> Maybe a) -> a -> [a]
        iterateMaybe f x =
          case f x of
            Nothing -> []
            Just x' -> x': iterateMaybe f x'


findPAWS :: TaggedLemma (Lemma ': as)
         -> ClauseTree
         -> VerbProperty (BitreeZipperICP (Lemma ': as))
         -> [X'Tree (Lemma ': as)]
         -> Maybe (PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag)))
findPAWS tagged tr vp x'tr = do
  cp <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
                                           -- anyway need to be rewritten.
  let rng = cpRange cp
  cp' <- (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng)) x'tr)
  predicateArgWS tagged cp' <$> findVerb (vp^.vp_index) tr <*> pure (cp' ^. complement.complement.adjunct)


---------
-- old --
---------


currentlevel :: Bitree (Range,(STag,Int)) t -> Int
currentlevel (PN (_,(_,l)) _) = l
currentlevel (PL _ )          = 0


promoteToVP :: ClauseTree -> Either (Int,(POSTag,Text)) ClauseTree
promoteToVP x@(PL (Right (i,(p,t)))) = if isVerb p || p == TO || p == MD
                                       then Left (i,(p,t))
                                       else Right x
promoteToVP x@(PL (Left _))          = Right x
promoteToVP (PN (_,(S_OTHER N.PRT,_)) (PL (Right (i,(p,t))):_)) = Left (i,(p,t))  -- for verb particle
promoteToVP x@(PN _ _)               = Right x


promote_PP_CP_from_NP :: Bitree (Range,(STag,Int)) t -> [Bitree (Range,(STag,Int)) t]
promote_PP_CP_from_NP x@(PN (_rng,(S_OTHER N.NP,_lvl)) [x1,x2]) =
  case (getRoot x1, getRoot x2) of
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_PP _ _ _,_)))   ->  [x1,x2]
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_SBAR _,_))) ->  [x1,x2]
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_CL _,_)))   ->  [x1,x2]
    _ -> [x]
promote_PP_CP_from_NP x = [x]



clauseStructure :: TaggedLemma '[Lemma]
                -> [VerbProperty (Zipper '[Lemma])]
                -> PennTreeIdxG N.CombinedTag (POSTag,Text)
                -> ClauseTree
clauseStructure _      _    (PL (i,pt)) = PL (Right (i,pt))
clauseStructure tagged vps  (PN (rng,tag) xs)
  = let ys = map (clauseStructure tagged vps) xs
        (verbs,nonverbs0)= partitionEithers (map promoteToVP ys)
        nonverbs = concatMap promote_PP_CP_from_NP nonverbs0
        lvl = maximum (map currentlevel ys) :: Int
    in case tag of
         N.CL c -> case c of
                     N.S    -> PN (rng,(S_CL c,lvl+1)) ys
                     N.SBAR ->
                       case xs of
                         PL (_,(IN,t))     : _ ->
                           case tail ys of
                             [] -> PL (Left (rng,(S_SBAR (SB_Word (IN,t)),lvl)))
                             _  -> PN (rng,(S_SBAR (SB_Word (IN,t)),lvl)) (tail ys)
                         PN (_,(N.PH p)) _ : _ ->
                           if N.isWHphrase p
                           then case tail ys of
                                  [] -> PL (Left (rng,(S_SBAR (SB_WH p),lvl)))
                                  _  -> PN (rng,(S_SBAR (SB_WH p),lvl)) (tail ys)
                           else PN (rng,(S_SBAR SB_None,lvl)) ys
                         _other                ->
                           PN (rng,(S_SBAR SB_None,lvl)) ys
                     _other -> PN (rng,(S_CL c,lvl)) ys
         N.PH p -> case p of
                     N.VP ->
                       case nonverbs of
                         [] -> PL (Left (rng,(S_VP verbs,lvl)))
                         _  -> PN (rng,(S_VP verbs,lvl)) nonverbs
                     N.PP ->
                       case xs of
                         -- prepositional case 1
                         PL (_,(IN,t)):os ->
                           -- we need to rewrite this whole functions using zipper later.
                           let prep = case os of
                                        PN (_,(N.CL N.S)) _lst : _ -> S_PP t PC_Other True
                                        PN (rng',_) _ : _           -> S_PP t (fromMaybe PC_Other (find (isMatchedTime rng') (tagged^.tagList) >> return PC_Time)) False
                                        PL (i,_) : _               -> S_PP t (fromMaybe PC_Other (find (isMatchedTime (i,i)) (tagged^.tagList) >> return PC_Time)) False
                                        _                          -> S_PP t PC_Other False
                           in case tail ys of
                                [] -> PL (Left (rng,(prep,lvl)))
                                _  -> PN (rng,(prep,lvl)) (tail ys)
                         -- prepositional case 2
                         PL (_,(TO,t)):os ->
                           -- we need to rewrite this whole functions using zipper later.
                           let prep = case os of
                                        PN (_,(N.CL N.S)) _lst : _ -> S_PP t PC_Other True
                                        PN (rng',_) _ : _           -> S_PP t (fromMaybe PC_Other (find (isMatchedTime rng') (tagged^.tagList) >> return PC_Time)) False
                                        PL (i,_) : _               -> S_PP t (fromMaybe PC_Other (find (isMatchedTime (i,i)) (tagged^.tagList) >> return PC_Time)) False
                                        _                          -> S_PP t PC_Other False
                           in case tail ys of
                                [] -> PL (Left (rng,(prep,lvl)))
                                _ -> PN (rng,(prep,lvl)) (tail ys)
                         -- non-prepositional case
                         _ -> PL (Left (rng,(S_PP "" PC_Other False,lvl)))
                     N.PRT ->
                       case xs of
                         PL (i,(p1,t)):_  -> PN (rng,(S_OTHER N.PRT,lvl)) [PL (Right (i,(p1,t)))]
                         _                -> PL (Left (rng,(S_OTHER p,lvl)))
                     _    -> PN (rng,(S_OTHER p,lvl)) ys
         N.RT   -> PN (rng,(S_RT,lvl)) ys


findVerb :: Int -> ClauseTree -> Maybe ClauseTreeZipper
findVerb i tr = getFirst (bifoldMap f f (mkBitreeZipper [] tr))
  where f x = First $ case getRoot (current x) of
                        Left (_,(S_VP lst,_))
                          -> if i `elem` (map (^._1) lst) then Just x else Nothing
                        Right (Left (_,(S_VP lst,_)))
                          -> if i `elem` (map (^._1) lst) then Just x else Nothing
                        _ -> Nothing



clauseRanges :: ClauseTree -> [Range]
clauseRanges tr = bifoldMap f (const []) tr
  where f (rng,(S_CL _,_)) = [rng]
        f _                = []




cutOutLevel0 :: ClauseTree -> ClauseTree
cutOutLevel0 x@(PL _             ) = x
cutOutLevel0 (PN (rng,(p,lvl)) xs) =
  if lvl == 0
  then case p of
         S_PP    _ _ _ -> PL (Left (rng,(p,lvl)))
         S_OTHER _     -> PL (Left (rng,(p,lvl)))
         _             -> PN (rng,(p,lvl)) (map cutOutLevel0 xs)
  else PN (rng,(p,lvl)) (map cutOutLevel0 xs)
