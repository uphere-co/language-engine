{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Lens.Extras                    (is)
import           Control.Monad                          ((<=<),(>=>),guard,void,when)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,execState,get,put,modify')
import           Data.Attribute                         (ahead)
import           Data.Bifoldable                        (bifoldMap)
import           Data.Bitraversable                     (bitraverse)
import           Data.Foldable                          (toList)
import           Data.Function                          (on)
import qualified Data.HashMap.Strict               as HM
import           Data.List                              (sortBy)
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (Last(..))
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           Data.ListZipper
import           Data.Range                             (rangeTree)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Voice(..))
--
import           NLP.Syntax.Noun                        (splitDP,mkPPFromZipper{- ,identifyInternalTimePrep -})
import           NLP.Syntax.Preposition                 (checkEmptyPrep,checkTimePrep)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs,isPOSAs,mergeLeftELZ,mergeRightELZ,rootTag)
--
import Debug.Trace
import qualified Data.Text as T
import NLP.Syntax.Format.Internal

data X'TreeState p = XTS { _xts_nextIndex :: Int
                         , _xts_tree      :: X'Tree p
                         }

makeLenses ''X'TreeState


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
    let cs_ord = siblingsBy (nextNotComma) checkCompVPness =<< maybeToList (headVP vprop)
        -- topicalized CP
        c_frag = do v <- headVP vprop
                    z <- firstSiblingBy next (\x -> rootTag x == Left FRAG) v
                    let rng = getRange (current z)
                    --trace ("c_frag1" ++ show rng) (return ())
                    guard (rootTag (current z) == Left FRAG)
                    --trace ("c_frag2" ++ show rng) (return ())
                    z' <- child1 z
                    --trace ("c_frag3" ++ show rng) (return ())
                    guard (checkCompVPness (current z'))
                    --trace ("c_frag4" ++ show rng) (return ())
                    return z'
        c_top = do guard (isChunkAs VP (current z_vp))
                   z_np <- prev z_vp
                   guard (isChunkAs NP (current z_np))
                   z_comma <- prev z_np
                   guard (isPOSAs M_COMMA (current z_comma))
                   z_cp <- prev z_comma
                   (guard (isChunkAs S (current z_cp) || isChunkAs SBAR (current z_cp)))
                   return z_cp
    in (maybe cs_ord (:cs_ord) c_frag, c_top)

  where
    nextNotComma z = do n <- next z
                        guard (not (isPOSAs M_COMMA (current n))) -- ad hoc separation using comma
                        return n
    checkCompVPness z = case rootTag z of
                          Left NP    -> True
                          Left PP    -> True
                          Left ADJP  -> True
                          Left SBAR  -> True
                          Left S     -> True
                          Left SBARQ -> True
                          Left SQ    -> True
                          -- Left FRAG  -> True
                          Left _     -> False
                          Right p    -> isNoun p == Yes || isAdjective p == Yes


complementsOfVerb :: forall (t :: [*]) (as :: [*]) . (t ~ (Lemma ': as)) =>
                     PreAnalysis t
                  -> VerbProperty (Zipper t)
                  -> Zipper t
                  -> ([Coindex (Either TraceType (Either Range (CompVP 'PH0)))]
                     ,Maybe (Coindex (Either TraceType (Either Range (CompVP 'PH0))))
                     ,[CPDPPP 'PH0]
                     )
complementsOfVerb tagged vprop z_vp =
  let (cs,mspec,dppps) = let (xs,mtop) = complementCandidates vprop z_vp
                             xs' = map xform xs ++
                                   maybeToList (mtop >>= \top -> do
                                     let rng_top = getRange (current top)
                                         comp = mkDefCoindex (Left Moved)
                                     return (comp,[]))
                             mspec' = mtop >>= \top -> do
                                        let rng_top = getRange (current top)
                                        return (mkDefCoindex (Right (Left rng_top)))
                         in (map (^._1) xs',mspec',concatMap (^._2) xs')
  in case vprop^.vp_voice of
       Active -> (cs,mspec,dppps)
       Passive -> (mkDefCoindex (Left Moved) : cs,mspec,dppps)
  where
    xform_dp z = let dptr@(DPTree dp' _pptrs) = splitDP tagged (DPTree (mkOrdDP z) [])
                     subs = getSubsFromDPTree dptr
                 in (mkDefCoindex (Right (Right (checkEmptyPrep tagged dp'))),subs)
    xform_pp z = fromMaybe (mkDefCoindex (Left NULL),[]) $ do
                   pptr <- mkPPFromZipper tagged z
                   let pp = pptr^._PPTree._1
                       subs = getSubsFromPPTree pptr
                   return (mkDefCoindex (Right (Right (checkTimePrep tagged pp))), subs)
    xform_cp z = (mkDefCoindex (Right (Left (getRange (current z)))), [])
    xform_ap z = let ap = mkAP (getRange (current z))
                 in  (mkDefCoindex (Right (Right (CompVP_AP ap))), [APCase ap])

    xform z = case rootTag (current z) of
                Left NP    -> xform_dp z
                Left PP    -> xform_pp z
                Left ADJP  -> xform_ap z
                Left _     -> xform_cp z  -- for the time being
                Right p    -> if | isNoun p      == Yes -> xform_dp z
                                 | isAdjective p == Yes -> xform_ap z
                                 | otherwise            -> xform_cp z




allAdjunctCPOfVerb :: VerbProperty (Zipper (Lemma ': as))
                   -> [Either Range (AdjunctCP 'PH0)]
allAdjunctCPOfVerb vprop =
    let mcomma = firstSiblingBy next (isPOSAs M_COMMA) =<< headVP vprop  -- ad hoc separation using comma
    in case mcomma of
         Nothing -> []
         Just comma -> map (Left . getRange . current) (siblingsBy next checkS comma)
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkS z = case tag z of
                    Left SBAR  -> True
                    Left S     -> True
                    Left SBARQ -> True
                    Left SQ    -> True
                    _          -> False



    -- for the time being, CP subject is not supported
identifySubject :: PreAnalysis (Lemma ': as)
                -> N.ClauseTag
                -> Zipper (Lemma ': as)   -- ^ Verb maximal projection
                -> (Coindex (Either TraceType (Either Range (SpecTP 'PH0))),[CPDPPP 'PH0])
identifySubject tagged tag vp = maybe nul smp r
  where
    r = case tag of
          N.SINV -> firstSiblingBy next (isChunkAs NP) vp          -- this should be refined.
          _      -> firstSiblingBy prev (isChunkAs NP) vp          -- this should be refined.
    nul = (mkDefCoindex (Left NULL),[])
    smp z = let dptr@(DPTree dp' _pptrs) = splitDP tagged (DPTree (mkOrdDP z) [])
                subs = getSubsFromDPTree dptr
            in (mkDefCoindex (Right (Right (SpecTP_DP dp'))),subs)


--
-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: PreAnalysis (Lemma ': as)
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP 'PH0,[CPDPPP 'PH0])
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
        let (comps,mtop,subs) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            (subj0,subs2) = identifySubject tagged s z_vp
            subj = fromMaybe subj0 $ do
                     dp <- subj0 ^? coidx_content._Right._Right._SpecTP_DP
                     let dptr@(DPTree dp' _) = splitDP tagged (DPTree dp [])
                         subj' = (coidx_content._Right._Right._SpecTP_DP .~ dp') subj0
                         -- subj_dps' = getSubsFromDPTree dptr
                     return subj'
            verbp = mkVerbP rng_vp (simplifyVProp vprop) [] comps
            ppdps = subs ++ subs2
        case cptag' of
          N.RT   ->
            let (cphead,cpspec) = case mtop of
                                    Just top -> (C_PHI,Just (mkDefCoindex (SpecCP_Topic top)))
                                    Nothing ->
                                      case prev z_tp of
                                        Nothing -> (C_PHI,Nothing)
                                        Just z -> if (isChunkAs WHNP (current z))
                                                  then (C_PHI,Just (mkDefCoindex (SpecCP_WH (getRange (current z)))))
                                                  else let cmpmntzr = case (listToMaybe . map (ahead . getAnnot . snd) . toList . current) z of
                                                                        Nothing -> C_PHI
                                                                        Just c -> C_WORD c
                                                       in (cmpmntzr, Nothing)
            in return (mkCP cphead rng_cp' cpspec adjs (mkTP rng_tp subj verbp),ppdps)
          N.CL N.SBAR ->
            let (cphead,cpspec) = case mtop of
                                    Just top -> (C_PHI,Just (mkDefCoindex (SpecCP_Topic top)))
                                    Nothing ->
                                      case prev z_tp of
                                        Nothing -> (C_PHI,Nothing)
                                        Just z -> if (isChunkAs WHNP (current z))
                                                  then (C_PHI,Just (mkDefCoindex (SpecCP_WH (getRange (current z)))))
                                                  else let cmpmntzr = case (listToMaybe . map (ahead . getAnnot . snd) . toList . current) z of
                                                                        Nothing -> C_PHI
                                                                        Just c -> C_WORD c
                                                       in (cmpmntzr,Nothing)
            in return (mkCP cphead rng_cp' cpspec adjs (mkTP rng_tp subj verbp),ppdps)
          N.CL _ ->
            return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),ppdps)
          _      -> -- somewhat problematic case?
            return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),ppdps)
      _ -> -- reduced relative clause
        let (comps,_,ppdps) = complementsOfVerb tagged vprop z_vp
            adjs  = allAdjunctCPOfVerb vprop
            verbp = mkVerbP rng_vp (simplifyVProp vprop) [] comps
            nullsubj = mkDefCoindex (Left NULL)
        in return (mkCP C_PHI rng_vp (Just (mkDefCoindex SpecCP_WHPHI)) adjs (mkTP rng_vp nullsubj verbp), ppdps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current


hierarchyBits :: PreAnalysis (Lemma ': as)
              -> (CP 'PH0, [CPDPPP 'PH0])
              -> Maybe [(Range, (Range, CPDPPP 'PH0))]
hierarchyBits _tagged (cp,subs) = do
  let rng = cp^.maximalProjection
      cpbit = (rng,(rng,CPCase cp))
      f x = let r = toRange x
            in (r,(r,x))
  return (cpbit:map f subs)



identifyCPHierarchy :: PreAnalysis (Lemma ': as)
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> [X'Tree 'PH0]
identifyCPHierarchy tagged vps = fromMaybe [] (traverse (bitraverse tofull tofull) rtr)
  where x'map = (HM.fromList . concat . mapMaybe (hierarchyBits tagged <=< constructCP tagged)) vps
        rngs = HM.keys x'map
        rangeSort (PN x xs) = PN x (sortBy (compare `on` (fst . getRoot1)) (map rangeSort xs))
        rangeSort (PL x)    = PL x
        rtr = map rangeSort (rangeTree rngs)
        tofull rng = HM.lookup rng x'map



currentCPDPPP :: X'Zipper p -> CPDPPP p
currentCPDPPP = snd . getRoot1 . current



issueIndex :: State (X'TreeState p) Int
issueIndex = do
  XTS i x'tr <- get
  put (XTS (i+1) x'tr)
  return i


updateTree :: X'Tree p -> State (X'TreeState p) ()
updateTree tr = modify' (xts_tree .~ tr)


{-
--
-- | rewrite X'Tree. Now it's focusing on modifier relation, but this should
--   be more generalized.
--
rewriteX'TreeForModifier :: ((Range, CPDPPP) -> (Range, CPDPPP)) -> X'Zipper -> DetP -> MaybeT (State X'TreeState) ()
rewriteX'TreeForModifier f w z = do
  i <- (^.xts_nextIndex) <$> lift get
  let dprng = z^.maximalProjection
      -- rewrite X'Tree by modifier relation.
  case extractZipperById dprng (toBitree w) of
    Nothing -> do let newtr (PN y ys) = PN (dprng,DPCase z) [PN (f y) ys]
                      newtr (PL y)    = PN (dprng,DPCase z) [PL (f y)]
                      w'' = replaceFocusTree newtr w
                  lift (put (XTS i (toBitree w'')))
    Just _  -> do let otr = case current w of
                              PN y ys -> PN (f y) ys
                              PL y    -> PL (f y)
                  lift . put . XTS i =<< hoistMaybe (removeFocusTree w)
                  w' <- MaybeT (extractZipperById dprng . (^.xts_tree) <$> get)
                  let newtr (PN y ys) = PN y (ys ++ [otr])
                      newtr (PL y)    = PN y [otr]
                      w'' =replaceFocusTree newtr w'
                  lift (put (XTS i (toBitree w'')))
-}


retrieveWCP :: Range -> MaybeT (State (X'TreeState 'PH0)) (X'Zipper 'PH0,CP 'PH0)
retrieveWCP rng = do
  tr <- (^.xts_tree) <$> lift get
  w <- hoistMaybe (extractZipperById rng tr)
  cp <- hoistMaybe (currentCPDPPP w ^? _CPCase)
  return (w,cp)


rewriteX'TreeForFreeWH :: Range -> X'Zipper 'PH0 -> DetP 'PH0 -> Maybe (X'Zipper 'PH0)
rewriteX'TreeForFreeWH rng w z' = do
  w_dom <- parent w
  cp_dom <- w_dom^?to currentCPDPPP._CPCase
  let vp_dom = cp_dom^.complement.complement
      comps = vp_dom^.complement
      comps' = flip map comps $ \comp -> fromMaybe comp $ do
                 rng_comp <- comp^?coidx_content._Right._Left
                 guard (rng_comp == rng)
                 return (mkDefCoindex (Right (Right (CompVP_DP z'))))
      rf = _2._CPCase.complement.complement.complement .~ comps'
  return (replaceFocusItem rf rf w_dom)



{- 
-- consider passive case only now for the time being.
connectRaisedDP :: Range -> MaybeT (State (X'TreeState 'PH0)) () -- X'Zipper
connectRaisedDP rng = do
  i <- (^.xts_nextIndex) <$> lift get
  (w,cp) <- retrieveWCP rng
  guard (cp ^. complement.complement.headX.vp_voice == Passive)
  c1:c2:[] <- return (cp^.complement.complement.complement)
  rng1 <- hoistMaybe (c1^?coidx_content._Right._Right._CompVP_DP.maximalProjection)
  cp' <- hoistMaybe (c2^?coidx_content._Right._Right._CompVP_CP)
  rng_dp <- hoistMaybe (cp'^?complement.specifier.coidx_content._Right._Right._SpecTP_DP.maximalProjection)
  when (rng1 == rng_dp) $ do
    let rf = (_2._CPCase.complement.specifier .~ emptyCoindex)
           . (_2._CPCase.complement.complement.complement .~ [c2])
        w' = replaceFocusItem rf rf w
    lift (put (XTS i (toBitree w')))
-}

movePassive :: (Int,CP 'PH1) -> (Int,CP 'PH1)
movePassive (i,cp) = fromMaybe (i,cp) $ do
  guard (cp^.complement.complement.headX.vp_voice == Passive)
  c:cs <- return (cp^.complement.complement.complement)
  let c' = (coidx_i .~ Just i) c
      cp'= cp & (complement.complement.complement .~ c':cs)
              . (complement.specifier.coidx_i .~ Just i)
  return (i+1,cp')


moveWH :: (Int,CP 'PH1) -> (Int,CP 'PH1)
moveWH (i,cp) = fromMaybe (i,cp) $ do
  let spec_cp = cp^.specifier
      spec_tp = cp^.complement.specifier
  guard $ fromMaybe False $
    spec_cp^?_Just.coidx_content >>= \case SpecCP_WHPHI -> return True
                                           SpecCP_WH _  -> return True
                                           _            -> return False
  case spec_tp^.coidx_content of
    Left NULL -> -- subject case
                 let mj = spec_tp^.coidx_i
                     i' = maybe (i+1) (const i) mj
                     j = fromMaybe i mj
                     cp' = cp & (complement.specifier .~ mkCoindex j (Left Moved))
                              . (specifier._Just.coidx_i .~ Just j)
                 in return (i',cp')
    Left _    -> return (i,cp)
    Right _   -> -- object case
                 let cp' = cp & (complement.complement.complement %~ (mkCoindex i (Left Moved):))
                              . (specifier._Just.coidx_i .~ Just i)
                 in return (i+1,cp')


bindingWH1 :: (X'Tree 'PH1) -> State Int (X'Tree 'PH1) -- X'Tree 'PH1 -> X'Tree 'PH1
bindingWH1 x'tr = bitraverse f f x'tr
 where f x = case x^._2 of
               CPCase cp -> do
                 i <- get
                 let (i',cp') = (moveWH . movePassive) (i,cp)
                 put i'
                 (return . (_2._CPCase .~ cp')) x
               _ -> return x



resolveWH :: X'Tree 'PH1 -> DetP 'PH1 -> DetP 'PH1
resolveWH x'tr dp = fromMaybe dp $ do
  np <- dp^.complement
  CompDP_CP rng_cp <- np^.complement
  w <- extractZipperById rng_cp x'tr
  cp <- currentCPDPPP w ^? _CPCase
  case cp^.specifier of
    Just (Coindex (Just i) SpecCP_WHPHI ) -> (return . (complement._Just.headX.coidx_i .~ Just i)) dp
    Just (Coindex (Just i) (SpecCP_WH _)) -> (return . (complement._Just.headX.coidx_i .~ Just i)) dp
    _                                     -> return dp

  

bindingWH2 :: X'Tree 'PH1 -> X'Tree 'PH1
bindingWH2 x'tr = bimap f f x'tr
 where f x = case x^._2 of
               DPCase dp -> 
                 let dp' = resolveWH x'tr dp
                 in (_2._DPCase .~ dp') x
               _ -> x


retrieveResolved :: X'Tree 'PH1 -> [(Int,Range,Range)]
retrieveResolved x'tr = bifoldMap f f x'tr
  where f x = case x^._2 of
                DPCase dp -> do hn <- dp^..complement._Just.headX
                                i <- hn^..coidx_i._Just
                                -- guard (hn^.coidx_i == Just i)
                                [(i,dp^.maximalProjection,hn^.coidx_content.hn_range)]
                CPCase cp -> do spectp <- cp^..complement.specifier
                                i <- spectp^..coidx_i._Just
                                -- guard (spectp^.coidx_i == Just i)
                                spectp1 <- spectp^..coidx_content._Right._SpecTP_DP
                                [(i,spectp1,spectp1)]
                _         -> []
                               
                               


--   bimap f f  where f = _2._CPCase %~ whMovement1

{-

whMovement :: PreAnalysis (Lemma ': as)
           -> (X'Zipper 'PH0,CP 'PH0)
           -> State (X'TreeState 'PH0) (Coindex (Either TraceType (Either Range (SpecTP 'PH0))))
whMovement tagged (w,cp) = do
  i <- (^.xts_nextIndex) <$> get
  -- letter z denotes zipper for PennTree, w denotes zipper for X'Tree
  let rng_cp = cp^.maximalProjection
      spec = cp^.complement.specifier
  -- whMovement process starts with cheking trace in subject
  case spec^.coidx_content of
    {- Left c -- (LZ ps c _ns) ->
      -- with trace in subject
      -- check subject for relative pronoun
      case c of -}
    Left NULL -> do
      -- ignore ns.
      -- let xspro = LZ ps PRO []
      fmap (fromMaybe (mkDefCoindex (Left PRO))) . runMaybeT $ do
        -- check subject position for relative pronoun
        z_cp <- hoistMaybe $ listToMaybe (extractZipperByRange rng_cp (tagged^.pennTree))  -- this can be dangerous
        ((do -- ordinary relative clause
             z_dp <- hoistMaybe (firstSiblingBy prev (isChunkAs NP) z_cp)
             let DPTree dp' _ = splitDP tagged (DPTree (mkOrdDP z_dp) [])  -- need to rewrite
             -- rewriteX'TreeForModifier id w dp'
             i <- lift issueIndex
             return (mkCoindex i (Left Moved)))              -- (SpecTP_DP dp')
         <|>
         (do -- free relative clause
             rng_wh <- hoistMaybe (cp^?specifier._Just._SpecCP_WH)
             z_wh <- hoistMaybe (listToMaybe (extractZipperByRange rng_wh (tagged^.pennTree)))
             let DPTree dp' _ = splitDP tagged (DPTree (mkOrdDP z_wh) [])   -- need to rewrite
             w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng_cp w dp')
             i <- lift issueIndex
             lift (updateTree (toBitree w_dom'))
             (w',_) <- retrieveWCP rng_cp
             -- rewriteX'TreeForModifier id w' dp'
             return (mkCoindex i (Left Moved)))) --  (SpecTP_DP dp')
    Left _    -> return spec
    Right _   -> do
      -- without trace in subject
      -- check object for relative pronoun
      void . runMaybeT $ do
        z_cp <- hoistMaybe $ listToMaybe (extractZipperByRange rng_cp (tagged^.pennTree))  -- this can be dangerous
        ((do -- ordinary relative clause
             z_dp <- hoistMaybe (firstSiblingBy prev (isChunkAs NP) z_cp)
             let DPTree dp' _ = splitDP tagged (DPTree (mkOrdDP z_dp) [])
             let -- adjust function for complement with relative pronoun resolution
                 -- rf0 = _2._CPCase.complement.complement.complement
                 --         %~ ((mkDefCoindex (Left Moved)):)   -- CompVP_DP dp'
             -- rewrite X'Tree for modifier relation.
             -- rewriteX'TreeForModifier rf0 w dp')
             return ())
         <|>
         (do -- free relative clause
             rng_wh <- hoistMaybe (cp^?specifier._Just._SpecCP_WH)
             z_wh <- hoistMaybe (listToMaybe (extractZipperByRange rng_wh (tagged^.pennTree)))
             let DPTree dp' _ = splitDP tagged (DPTree (mkOrdDP z_wh) [])
             w_dom' <- hoistMaybe (rewriteX'TreeForFreeWH rng_cp w dp')
             lift (put (XTS i (toBitree w_dom')))
             (w',_) <- retrieveWCP rng_cp
             let -- adjust function for complement with relative pronoun resolution
                 -- rf0 = _2._CPCase.complement.complement.complement
                 --        %~ ((mkDefCoindex (Left Moved)):)  --  CompVP_DP dp'
             -- rewriteX'TreeForModifier rf0 w' dp'))
             return ()))
      return spec


-}


resolvePRO :: PreAnalysis (Lemma ': as)
           -> (X'Zipper 'PH0,CP 'PH0)
           -> MaybeT (State (X'TreeState 'PH0)) (Coindex (Either TraceType (Either Range (SpecTP 'PH0))))
resolvePRO tagged (z,cp) = do
  -- XTS i x'tr <- lift get
  let spec = cp^.complement.specifier
  case spec^.coidx_content of
    Left NULL -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                      let rng_cp' = cp'^.maximalProjection
                      -- Coindex exs' x' <- lift (resolveDP tagged rng_cp')
                      i <- lift issueIndex
                      -- lift (put (XTS (i+1) x'tr))
                      return (mkCoindex i (Left PRO)))
                  <|>
                  return (mkDefCoindex (Left PRO)))
    Left PRO  -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDPPP =<< parent z)
                      let rng_cp' = cp'^.maximalProjection
                      -- Coindex exs' x' <- lift (resolveDP tagged rng_cp')
                      return (mkDefCoindex (Left PRO))) -- (mergeLeftELZ (Left xs) exs') x'
                  <|>
                  return (mkDefCoindex (Left NULL)))
    _   -> return spec


--
-- | resolve passive DP-movement. this is ad hoc yet.
--
resolveVPComp :: Range
              -> Coindex (Either TraceType (Either Range (SpecTP 'PH0)))
              -> MaybeT (State (X'TreeState 'PH0)) (Coindex (Either TraceType (Either Range (SpecTP 'PH0))))
resolveVPComp rng spec = do
  i <- (^.xts_nextIndex) <$> lift get
  (w,cp) <- retrieveWCP rng
  let verbp = cp^.complement.complement
  case verbp^.headX.vp_voice of
    Active -> return spec
    Passive -> do
      let cs = verbp^.complement
      case cs of
        [] -> trace "No complements?" $ return spec
        c:rest -> do
          let r :: Maybe (CompVP 'PH0)
              r = (\case SpecTP_DP dp -> Just (CompVP_DP dp)) =<< spec^?coidx_content._Right._Right  -- ignore CP case for the time being.
              c' = c -- (mergeLeftELZ (c^.trChain) (spec^.trChain)) r
              rf = _2._CPCase.complement.complement.complement .~ (c':rest)
              w' = replaceFocusItem rf rf w
          lift (put (XTS i (toBitree w')))
          return spec --  -- (mergeRightELZ (c^.trChain) (spec^.trChain)) (spec^.coidx_content))


--
-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
{- 
resolveDP :: PreAnalysis (Lemma ': as)
          -> Range
          -> State (X'TreeState 'PH0) (Coindex (Either TraceType (Either Range (SpecTP 'PH0))))
resolveDP tagged rng = fmap (fromMaybe emptyCoindex) . runMaybeT $ do
  (w,cp) <- retrieveWCP rng
  if is _Just (cp^.specifier)  -- relative clause
    then resolveVPComp rng =<< lift (whMovement tagged (w,cp))
    else resolveVPComp rng =<< resolvePRO tagged (w,cp)

-}

{- 
--
-- | Resolve unbound CP argument to bound CP argument.
--
resolveCP :: X'TreeState 'PH0 -> X'TreeState 'PH0
resolveCP = rewriteTree action
  where
    debugfunc msg = do xtr' <- (^.xts_tree) <$> lift get
                       trace ("\n" ++ msg ++ "\n" ++ T.unpack (formatX'Tree xtr')) $ return ()

    action rng = do z <- hoistMaybe . extractZipperById rng . (^.xts_tree) =<< lift get
                    ((replace z >> return ()) <|> return ())
    --
    -- replace :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replace = replaceSpecCP >=> replaceCompVP >=> replaceAdjunctCP
    --
    -- I need to deduplicate the following code.
    putAndReturn w = do
      i <- (^.xts_nextIndex) <$> lift get
      lift (put (XTS i (toBitree w)))
      return w

    --
    -- replaceCompVP :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replaceCompVP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let rng = cp^.maximalProjection
      let xs = cp^.complement.complement.complement
      xs' <- flip traverse xs $ \x ->
               ((do tr <- (^.xts_tree) <$> lift get
                    rng_compvp <- hoistMaybe (x^?coidx_content._Right._Left)
                    y <- hoistMaybe (extractZipperById rng_compvp tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (coidx_content .~ Right (Right (CompVP_CP cp')))) x
                )
                <|>
                (return x))
      -- flip traverse xs' $ \x -> do
      --   trace ("\nreplaceCompVP7 " ++ T.unpack (formatCoindex formatCompVP x) ) (return ())
      let rf = _2._CPCase.complement.complement.complement .~ xs'
      z' <- hoistMaybe . extractZipperById rng . (^.xts_tree) =<< lift get
      w' <- putAndReturn (replaceFocusItem rf rf z')
      -- debugfunc "replaceCompVP8"

      -- trace ("\nreplaceCompVP8 :\n" ++ T.unpack (formatX'Tree xtr')) $ return ()

      return w'
    --
    -- replaceSpecCP :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replaceSpecCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let mx = cp^?specifier._Just._SpecCP_Topic
      mx' <- flip traverse mx $ \x ->
               ((do
                    tr <- (^.xts_tree) <$> lift get
                    rng <- hoistMaybe (x^?coidx_content._Right._Left)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (coidx_content .~ Right (Right (CompVP_CP cp')))) x
                )
                <|>
                (return x))
      let rf = _2._CPCase.specifier .~ fmap SpecCP_Topic mx'
      putAndReturn (replaceFocusItem rf rf z)
    --
    -- replaceAdjunctCP :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replaceAdjunctCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let xs = cp^.adjunct
      xs' <- flip traverse xs $ \x ->
               ((do tr <- (^.xts_tree) <$> lift get
                    rng <- hoistMaybe (x^?_Left)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    return (Right (AdjunctCP_CP cp'))
                )
                <|>
                (return x))
      let rf = _2._CPCase.adjunct .~ xs'
      putAndReturn (replaceFocusItem rf rf z)
-}


bindingSpec :: Range
            -> Coindex (Either TraceType (Either Range (SpecTP 'PH0)))
            -> MaybeT (State (X'TreeState 'PH0)) () -- X'Zipper
bindingSpec rng spec = do
  XTS i x'tr <- lift get
  z <- hoistMaybe (extractZipperById rng x'tr)
  let rf = _2._CPCase.complement.specifier .~ spec
      z' = replaceFocusItem rf rf z
  lift (put (XTS i (toBitree z')))
  -- return z'

{- 
-- consider passive case only now for the time being.
connectRaisedDP :: Range -> MaybeT (State (X'TreeState 'PH0)) () -- X'Zipper
connectRaisedDP rng = do
  i <- (^.xts_nextIndex) <$> lift get
  (w,cp) <- retrieveWCP rng
  guard (cp ^. complement.complement.headX.vp_voice == Passive)
  c1:c2:[] <- return (cp^.complement.complement.complement)
  rng1 <- hoistMaybe (c1^?coidx_content._Right._Right._CompVP_DP.maximalProjection)
  cp' <- hoistMaybe (c2^?coidx_content._Right._Right._CompVP_CP)
  rng_dp <- hoistMaybe (cp'^?complement.specifier.coidx_content._Right._Right._SpecTP_DP.maximalProjection)
  when (rng1 == rng_dp) $ do
    let rf = (_2._CPCase.complement.specifier .~ emptyCoindex)
           . (_2._CPCase.complement.complement.complement .~ [c2])
        w' = replaceFocusItem rf rf w
    lift (put (XTS i (toBitree w')))
-}

-- I think we should change the name of these bindingAnalysis.. functions.

{- 
--
-- | This is the final step to bind inter-clause trace chain
--
bindingAnalysis :: PreAnalysis (Lemma ': as) -> X'TreeState 'PH0 -> X'TreeState 'PH0
bindingAnalysis tagged = rewriteTree $ \rng -> lift (resolveDP tagged rng) >>= bindingSpec rng
-}

{-
--
-- |
--
bindingAnalysisRaising :: X'TreeState 'PH0 -> X'TreeState 'PH0
bindingAnalysisRaising = rewriteTree (\rng -> connectRaisedDP rng <|> return ())
-}

--
-- | This is a generic tree-rewriting operation.
--   It assumes range index is not changed after each operation
--
rewriteTree :: (Range -> MaybeT (State (X'TreeState 'PH0)) ()) -> X'TreeState 'PH0 -> X'TreeState 'PH0
rewriteTree action xts = execState (go rng0) xts
  where getrng = fst . getRoot1 . current
        rng0 = (either fst fst . getRoot . (^.xts_tree)) xts
        go rng = void . runMaybeT $ do
                   action rng
                   w <- hoistMaybe . extractZipperById rng . (^.xts_tree) =<< lift get
                   (((do
                        w' <- hoistMaybe (child1 w)  -- depth first
                        lift (go (getrng w')))
                     <|> return ())
                    >>
                    ((do
                         w'' <- hoistMaybe (next w)
                         lift (go (getrng w'')))
                     <|> return ()))




mkX'TreePH1 :: X'Tree 'PH0 -> X'Tree 'PH1
mkX'TreePH1 x'tr = bimap f f x'tr
  where
    f (rng,x) = (rng,mkCPDPPPPH1 x)


{-  where
    replaceCompVP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let rng = cp^.maximalProjection
      let xs = cp^.complement.complement.complement
      xs' <- flip traverse xs $ \x ->
               ((do tr <- (^.xts_tree) <$> lift get
                    rng_compvp <- hoistMaybe (x^?coidx_content._Right._Left)
                    y <- hoistMaybe (extractZipperById rng_compvp tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (coidx_content .~ Right (Right (CompVP_CP cp')))) x
                )
                <|>
                (return x))
      -- flip traverse xs' $ \x -> do
      --   trace ("\nreplaceCompVP7 " ++ T.unpack (formatCoindex formatCompVP x) ) (return ())
      let rf = _2._CPCase.complement.complement.complement .~ xs'
      z' <- hoistMaybe . extractZipperById rng . (^.xts_tree) =<< lift get
      w' <- putAndReturn (replaceFocusItem rf rf z')
      -- debugfunc "replaceCompVP8"

      -- trace ("\nreplaceCompVP8 :\n" ++ T.unpack (formatX'Tree xtr')) $ return ()

      return w'
    --
    -- replaceSpecCP :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replaceSpecCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let mx = cp^?specifier._Just._SpecCP_Topic
      mx' <- flip traverse mx $ \x ->
               ((do
                    tr <- (^.xts_tree) <$> lift get
                    rng <- hoistMaybe (x^?coidx_content._Right._Left)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    (return . (coidx_content .~ Right (Right (CompVP_CP cp')))) x
                )
                <|>
                (return x))
      let rf = _2._CPCase.specifier .~ fmap SpecCP_Topic mx'
      putAndReturn (replaceFocusItem rf rf z)
    --
    -- replaceAdjunctCP :: X'Zipper -> MaybeT (State X'TreeState) X'Zipper
    replaceAdjunctCP z = do
      cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
      let xs = cp^.adjunct
      xs' <- flip traverse xs $ \x ->
               ((do tr <- (^.xts_tree) <$> lift get
                    rng <- hoistMaybe (x^?_Left)
                    y <- hoistMaybe (extractZipperById rng tr)
                    y' <- replace y
                    cp' <- hoistMaybe (y' ^? to current . to getRoot1 . _2 . _CPCase)
                    return (Right (AdjunctCP_CP cp'))
                )
                <|>
                (return x))
      let rf = _2._CPCase.adjunct .~ xs'
      putAndReturn (replaceFocusItem rf rf z)

-}
