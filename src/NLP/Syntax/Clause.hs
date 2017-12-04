{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
import           Data.Bitraversable                     (bitraverse)
import           Data.Foldable                          (toList)
import qualified Data.HashMap.Strict               as HM
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (Last(..))
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
import           NLP.Syntax.Noun                        (splitDP,mkPPFromZipper,identifyInternalTimePrep)
import           NLP.Syntax.Preposition                 (checkEmptyPrep,checkTimePrep)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs,isPOSAs,mergeLeftELZ,mergeRightELZ,rootTag)
--
import Debug.Trace



hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return


maximalProjectionVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
maximalProjectionVP vp = listToMaybe (vp^.vp_words) >>= parent . fst


parentOfVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
parentOfVP vp = parent =<< maximalProjectionVP vp


headVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
headVP vp = getLast (mconcat (map (Last . Just . fst) (vp^.vp_words)))


complementCandidates :: forall (t :: [*]) (as :: [*]) . (t ~ (Lemma ': as)) =>
                        VerbProperty (Zipper t) -> Zipper t -> ([Zipper t],Maybe (Zipper t))
complementCandidates vprop z_vp =
    let cs_ord = siblingsBy (nextNotComma) checkNPSBAR =<< maybeToList (headVP vprop)
        -- topicalized CP
        c_top = do guard (isChunkAs VP (current z_vp))
                   z_np <- prev z_vp
                   guard (isChunkAs NP (current z_np))
                   z_comma <- prev z_np
                   guard (isPOSAs M_COMMA (current z_comma))
                   z_cp <- prev z_comma
                   (guard (isChunkAs S (current z_cp) || isChunkAs SBAR (current z_cp)))
                   return z_cp
    in (cs_ord,c_top)

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


complementsOfVerb :: forall (t :: [*]) (as :: [*]) . (t ~ (Lemma ': as)) =>
                     TaggedLemma t
                  -> VerbProperty (Zipper t)
                  -> Zipper t
                  -> ([TraceChain (CompVP t)],Maybe (TraceChain (CompVP t)), [AdjunctVP t])
complementsOfVerb tagged vprop z_vp =
  let (cs,mspec,adjs) = let (xs,mtop) = complementCandidates vprop z_vp
                            xs' = map xform xs ++
                                  maybeToList (mtop >>= \top -> do
                                                 let rng_top = getRange (current top)
                                                 return (TraceChain (Left (singletonLZ Moved)) (Just (CompVP_Unresolved rng_top)), []))
                            mspec' = mtop >>= \top -> do
                                                 let rng_top = getRange (current top)
                                                 return (TraceChain (Right [Moved]) (Just (CompVP_Unresolved rng_top)))
                        in (map fst xs',mspec',concatMap snd xs')
  in case vprop^.vp_voice of
       Active -> (cs,mspec,adjs)
       Passive -> (TraceChain (Left (singletonLZ Moved)) Nothing : cs,mspec,adjs)
  where
    xform_dp z = let dp = splitDP tagged (mkOrdDP z)
                     (dp',zs) = identifyInternalTimePrep tagged dp
                 in (TraceChain (Right []) (Just (checkEmptyPrep tagged dp')), zs)
    xform_pp z = (TraceChain (Right []) (checkTimePrep tagged <$> mkPPFromZipper tagged PC_Other z), [])
      -- checkTimePrep tagged

    xform_cp z = (TraceChain (Right []) (Just (CompVP_Unresolved (getRange (current z)))), [])
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
         Just comma -> map (AdjunctCP_Unresolved . getRange . current) (siblingsBy next checkS comma)
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkS z = case tag z of
                    Left SBAR  -> True
                    Left S     -> True
                    Left SBARQ -> True
                    Left SQ    -> True
                    _          -> False



    -- for the time being, CP subject is not supported
identifySubject :: TaggedLemma (Lemma ': as)
                -> N.ClauseTag
                -> Zipper (Lemma ': as)   -- ^ Verb maximal projection
                -> (TraceChain (SpecTP (Lemma ': as)), [AdjunctVP (Lemma ': as)])
identifySubject tagged tag vp = maybe nul smp r
  where
    r = case tag of
          N.SINV -> firstSiblingBy next (isChunkAs NP) vp          -- this should be refined.
          _      -> firstSiblingBy prev (isChunkAs NP) vp          -- this should be refined.
    nul = (TraceChain (Left (singletonLZ NULL)) Nothing,[])
    smp z = let dp = splitDP tagged (mkOrdDP z)
                (dp',adjs) = identifyInternalTimePrep tagged dp
            in (TraceChain (Right []) (Just (SpecTP_DP dp')),adjs)


--
-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: TaggedLemma (Lemma ': as)
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as),[SpecTP (Lemma ': as)])
constructCP tagged vprop = do
    z_vp <- maximalProjectionVP vprop
    let rng_vp = getRange (current z_vp)
    z_tp <- parentOfVP vprop
    let rng_tp = getRange (current z_tp)
    tptag' <- N.convert <$> getchunk z_tp
    case tptag' of
      N.CL s -> do
        z_cp' <- parent z_tp
        let rng_cp' = getRange (current z_cp')
        cptag' <- N.convert <$> getchunk z_cp'
        let (comps,mtop,cadjs) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToSpecTP)
            (subj0,sadjs) = identifySubject tagged s z_vp
            subj = ((trResolved . _Just . _SpecTP_DP) %~ splitDP tagged) subj0
            subj_dps = subj^..trResolved._Just
            verbp = mkVerbP rng_vp vprop (cadjs++sadjs) comps
            dps = subj_dps ++ comps_dps
        case cptag' of
          N.RT   ->
            let (cphead,cpspec) = case mtop of
                                    Just top -> (C_PHI,Just (SpecCP_Topic top))
                                    Nothing ->
                                      case prev z_tp of
                                        Nothing -> (C_PHI,Nothing)
                                        Just z -> if (isChunkAs WHNP (current z))
                                                  then (C_PHI,Just (SpecCP_WH z))
                                                  else let cmpmntzr = case (listToMaybe . map (tokenWord . snd) . toList . current) z of
                                                                        Nothing -> C_PHI
                                                                        Just c -> C_WORD c
                                                       in (cmpmntzr, Nothing)
            in return (mkCP cphead rng_cp' cpspec adjs (mkTP rng_tp subj verbp),dps)
          N.CL N.SBAR ->
            let (cphead,cpspec) = case mtop of
                                    Just top -> (C_PHI,Just (SpecCP_Topic top))
                                    Nothing ->
                                      case prev z_tp of
                                        Nothing -> (C_PHI,Nothing)
                                        Just z -> if (isChunkAs WHNP (current z))
                                                  then (C_PHI,Just (SpecCP_WH z))
                                                  else let cmpmntzr = case (listToMaybe . map (tokenWord . snd) . toList . current) z of
                                                                        Nothing -> C_PHI
                                                                        Just c -> C_WORD c
                                                       in (cmpmntzr,Nothing)
            in return (mkCP cphead rng_cp' cpspec adjs (mkTP rng_tp subj verbp),dps)
          N.CL _ ->
            return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),dps)
          _      -> -- somewhat problematic case?
            return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),dps)
      _ -> -- reduced relative clause
        let (comps,_,cadjs) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToSpecTP)
            verbp = mkVerbP rng_vp vprop cadjs comps
            nullsubj = TraceChain (Left (singletonLZ NULL)) Nothing
        in return (mkCP C_PHI rng_vp (Just SpecCP_WHPHI) adjs (mkTP rng_vp nullsubj verbp),comps_dps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current


hierarchyBits :: TaggedLemma (Lemma ': as)
              -> (CP (Lemma ': as), [DetP (Lemma ': as)])
              -> Maybe [(Range, (Range, CPDPPP (Lemma ': as)))]
hierarchyBits tagged (cp,dps) = do
  let rng = cp^.maximalProjection
      cpbit = (rng,(rng,CPCase cp))
      f dp = let rng' = dp^.maximalProjection
                 dpbit = (rng',(rng',DPCase dp))
                 lst = do adj <- dp^.adjunct
                          case adj of
                            AdjunctDP_PP pp ->
                              let rng_pp = pp^.maximalProjection
                              in return (rng_pp,(rng_pp,PPCase pp))
                            AdjunctDP_Unresolved rng_pp -> maybeToList $ do
                              z_pp <- find (isChunkAs PP . current) (extractZipperByRange rng_pp (tagged^.pennTree))
                              (rng_pp,) . (rng_pp,) . PPCase <$> mkPPFromZipper tagged PC_Other z_pp
             in dpbit : lst
  return (cpbit:concatMap f dps)



identifyCPHierarchy :: TaggedLemma (Lemma ': as)
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> [X'Tree (Lemma ': as)]
identifyCPHierarchy tagged vps = fromMaybe [] (traverse (bitraverse tofull tofull) rtr)
  where x'map = (HM.fromList . concat . mapMaybe (hierarchyBits tagged . (_2 %~ (\x -> x^..traverse._SpecTP_DP)) <=< constructCP tagged)) vps
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
                 rng_comp <- comp^?trResolved._Just._CompVP_Unresolved
                 guard (rng_comp == rng)
                 return (TraceChain (Right (ps++[Moved,WHPRO])) (Just (CompVP_DP z')))
      rf = _2._CPCase.complement.complement.complement .~ comps'
  return (replaceFocusItem rf rf w_dom)


whMovement :: TaggedLemma (Lemma ': as)
           -> (X'Zipper (Lemma ': as),CP (Lemma ': as))
           -> State (X'Tree (Lemma ': as))
                    (TraceChain (SpecTP (Lemma ': as)))
whMovement tagged (w,cp) = do
  -- letter z denotes zipper for PennTree, w denotes zipper for X'Tree
  let rng_cp = cp^.maximalProjection
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
            z_cp <- hoistMaybe $ listToMaybe (extractZipperByRange rng_cp (tagged^.pennTree))  -- this can be dangerous
            ((do -- ordinary relative clause
                 dp' <- splitDP tagged . mkOrdDP <$> hoistMaybe (firstSiblingBy prev (isChunkAs NP) z_cp)
                 rewriteX'TreeForModifier id w dp'
                 return (TraceChain (Left (LZ ps Moved [WHPRO])) (Just (SpecTP_DP dp'))))
             <|>
             (do -- free relative clause
                 dp' <- splitDP tagged . mkOrdDP <$> hoistMaybe (cp^?specifier._Just._SpecCP_WH)
                 w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng_cp (reverse ps) w dp')
                 lift (put (toBitree w_dom'))
                 (w',_) <- retrieveWCP rng_cp
                 rewriteX'TreeForModifier id w' dp'
                 return (TraceChain (Left (LZ ps Moved [WHPRO])) (Just (SpecTP_DP dp')))))
        _    -> return spec
    Right ps -> do
      -- without trace in subject
      -- check object for relative pronoun
      void . runMaybeT $ do
        z_cp <- hoistMaybe $ listToMaybe (extractZipperByRange rng_cp (tagged^.pennTree))  -- this can be dangerous
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
             w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng_cp ps w z')
             lift (put (toBitree w_dom'))
             (w',_) <- retrieveWCP rng_cp
             let -- adjust function for complement with relative pronoun resolution
                 rf0 = _2._CPCase.complement.complement.complement
                         %~ (TraceChain (Left (LZ [] Moved [WHPRO])) (Just (CompVP_DP z')) :)
             rewriteX'TreeForModifier rf0 w' z'))
      return spec


resolveSilentPRO :: TaggedLemma (Lemma ': as)
                 -> (X'Zipper (Lemma ': as),CP (Lemma ': as))
                 -> MaybeT (State (X'Tree (Lemma ': as))) (TraceChain (SpecTP (Lemma ': as)))
resolveSilentPRO tagged (z,cp) = do
  let spec = cp^.complement.specifier
  case spec^.trChain of
    Right _ -> return spec
    Left (xs@(LZ _ c _)) -> case c of
      NULL      -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                        let rng_cp' = cp'^.maximalProjection
                        TraceChain exs' x' <- lift (resolveDP tagged rng_cp')
                        return (TraceChain (mergeLeftELZ (Left (replaceLZ SilentPRO xs)) exs') x'))
                    <|>
                    return (TraceChain (Left (replaceLZ SilentPRO xs)) Nothing))
      SilentPRO -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                        let rng_cp' = cp'^.maximalProjection
                        TraceChain exs' x' <- lift (resolveDP tagged rng_cp')
                        return (TraceChain (mergeLeftELZ (Left xs) exs') x'))
                    <|>
                    return (TraceChain (Left xs) Nothing))
      _         -> return spec

--
-- | resolve passive DP-movement. this is ad hoc yet.
--
resolveVPComp :: Range
              -> TraceChain (SpecTP (Lemma ': as))
              -> MaybeT (State (X'Tree (Lemma ': as))) (TraceChain (SpecTP (Lemma ': as)))
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
          let r = (\case SpecTP_Unresolved _ -> Nothing; SpecTP_DP dp -> Just (CompVP_DP dp)) =<< spec^.trResolved  -- ignore CP case for the time being.
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
          -> State (X'Tree (Lemma ': as)) (TraceChain (SpecTP (Lemma ': as)))
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
    replace = replaceSpecCP >=> replaceCompVP >=> replaceAdjunctCP
    --
    -- I need to deduplicate the following code.
    replaceSpecCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let mx = cp^?specifier._Just._SpecCP_Topic
      mx' <- flip traverse mx $ \x ->
               ((do
                    tr <- lift get
                    rng <- hoistMaybe (x^?trResolved._Just._CompVP_Unresolved)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (trResolved .~ Just (CompVP_CP cp'))) x
                )
                <|>
                (return x))
      let rf = _2._CPCase.specifier .~ fmap SpecCP_Topic mx'
          z' = replaceFocusItem rf rf z
      lift (put (toBitree z'))
      return z'
    --
    replaceCompVP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let xs = cp^.complement.complement.complement
      xs' <- flip traverse xs $ \x ->
               ((do tr <- lift get
                    rng <- hoistMaybe (x^?trResolved._Just._CompVP_Unresolved)
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
                    rng <- hoistMaybe (x^?_AdjunctCP_Unresolved)
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


bindingSpec :: Range -> TraceChain (SpecTP t) -> MaybeT (State (X'Tree t)) (X'Zipper t)
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
  rng1 <- hoistMaybe (c1^?trResolved._Just._CompVP_DP.maximalProjection)
  cp' <- hoistMaybe (c2^?trResolved._Just._CompVP_CP)
  rng_dp <- hoistMaybe (cp'^?complement.specifier.trResolved._Just._SpecTP_DP.maximalProjection)
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
