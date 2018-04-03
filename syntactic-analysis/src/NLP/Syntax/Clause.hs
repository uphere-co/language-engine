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
import           Control.Monad                          ((<=<),guard)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,get,put,modify')
import           Data.Attribute                         (ahead)
import           Data.Bifoldable                        (biList)
import           Data.Bifunctor                         (second)
import           Data.Bitraversable                     (bitraverse)
import           Data.Foldable                          (toList)
import           Data.Function                          (on)
import qualified Data.HashMap.Strict               as HM
import           Data.List                              (partition,sortBy)
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (Last(..))
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           Data.Range                             (rangeTree)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Voice(..))
--
import           NLP.Syntax.Noun                        (splitDP,mkPPFromZipper{- ,identifyInternalTimePrep -})
import           NLP.Syntax.Preposition                 (checkEmptyPrep,checkTimePrep)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs,isPOSAs,rootTag)
--



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
                    guard (rootTag (current z) == Left FRAG)
                    z' <- child1 z
                    guard (checkCompVPness (current z'))
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
                     ,Maybe (Coindex SpecCP)
                     ,[CPDPPP 'PH0]
                     )
complementsOfVerb tagged vprop z_vp =
  let (cs,mspec,dppps) = let (xs,mtop) = complementCandidates vprop z_vp
                             xs' = map xform xs ++
                                   maybeToList (mtop >>= \_ -> do
                                     return (mkDefCoindex (Left Moved),[]))
                             mspec' = mtop >>= \top -> do
                                        let rng_top = getRange (current top)
                                        return (mkDefCoindex (SpecCP_Topic (SpecTopicP_CP rng_top)))
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
allAdjunctCPOfVerb vprop = fromMaybe [] $ do
    comma1 <- firstSiblingBy next (isPOSAs M_COMMA) =<< headVP vprop  -- ad hoc separation using comma
    ((do comma2 <- firstSiblingBy next (isPOSAs M_COMMA) comma1  -- parenthetical. inside parenthesis is not yet handled.
         let x = map (Left . getRange . current) (siblingsBy next checkS comma2)
         return x
         )
     <|>
     (return (map (Left . getRange . current) (siblingsBy next checkS comma1))))
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
                     let DPTree dp' _ = splitDP tagged (DPTree dp [])
                     return ((coidx_content._Right._Right._SpecTP_DP .~ dp') subj0)
            verbp = mkVerbP rng_vp (simplifyVProp vprop) [] comps
            ppdps = subs ++ subs2
        case cptag' of
          N.RT   ->
            let (cphead,cpspec) = case mtop of
                                    Just top -> (C_PHI,Just top)
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
                                    Just top -> (C_PHI,Just top)
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
          N.CL _ -> return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),ppdps)
          _      -> return (mkCP C_PHI rng_tp Nothing adjs (mkTP rng_tp subj verbp),ppdps)
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



resolveCP :: X'Tree 'PH0 -> X'Tree 'PH0
resolveCP x'tr = bimap rf rf x'tr
  where
    x'map = biList x'tr
    --
    rf (rng,(CPCase cp)) = (rng,CPCase (replace cp))
    rf x                 = x
    --
    replace :: CP 'PH0 -> CP 'PH0
    replace = replaceAdjunctCP . replaceCompVP
    --
    resolve _ (Right x)  = Right x
    resolve h (Left rng) = case lookup rng x'map of
                             Nothing -> Left rng
                             Just cpdppp -> case h cpdppp of
                                              Nothing -> Left rng
                                              Just y  -> Right y
    --
    replaceCompVP cp =
      let xs = cp^.complement.complement.complement
          (as,xs') =  (partition f . map (coidx_content %~ second (resolve (Just . cpdpppToCompVP)))) xs
            where f x = fromMaybe False $ do
                          cp1 <- x^?coidx_content._Right._Right._CompVP_CP
                          (/= "that") <$> cp1^?headX._C_WORD.to unLemma
          as' = mapMaybe f as
            where f x = do cp1 <- x^?coidx_content._Right._Right._CompVP_CP
                           return (Right (AdjunctCP_CP cp1))
      in cp & (adjunct %~ (++as'))
            . (complement.complement.complement .~ xs')
    --
    replaceAdjunctCP cp =
      let xs = cp^.adjunct
          xs' = map (resolve cpdpppToAdjunctCP) xs
      in cp & (adjunct .~ xs')


issueIndex :: State (X'TreeState p) Int
issueIndex = do
  XTS i x'tr <- get
  put (XTS (i+1) x'tr)
  return i


updateTree :: X'Tree p -> State (X'TreeState p) ()
updateTree tr = modify' (xts_tree .~ tr)



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


moveTopic :: (Int,CP 'PH1) -> (Int, CP 'PH1)
moveTopic (i,cp) = fromMaybe (i,cp) $ do
  t <- cp^?specifier._Just
  guard (case t^.coidx_content of SpecCP_Topic _ -> True; _ -> False)
  let t' = (coidx_i .~ Just i) t
      rf c = case c^.coidx_content of
               Left Moved -> c & coidx_i .~ Just i
               _          -> c
      cp' = cp & (specifier._Just .~ t') .  (complement.complement.complement.traverse %~ rf)
  return (i+1,cp')


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


markPRO :: (Int,CP 'PH1) -> (Int,CP 'PH1)
markPRO (i,cp) = fromMaybe (i,cp) $ do
  let spec_tp = cp^.complement.specifier
  Left NULL <- return (spec_tp^.coidx_content)
  let mj = spec_tp^.coidx_i
      i' = maybe (i+1) (const i) mj
      j = fromMaybe i mj
  let cp' = cp & (complement.specifier .~ mkCoindex j (Left PRO))
  return (i',cp')


bindingWH1 :: (X'Tree 'PH1) -> State Int (X'Tree 'PH1)
bindingWH1 x'tr = bitraverse f f x'tr
 where f x = case x^._2 of
               CPCase cp -> do
                 i <- get
                 let (i',cp') = (markPRO . moveWH . movePassive . moveTopic) (i,cp)
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


resolvePRO :: X'Tree 'PH1 -> CP 'PH1 -> CP 'PH1
resolvePRO x'tr cp = fromMaybe cp $ do
  let rng_cp = cp^.maximalProjection
  case cp^.complement.specifier.coidx_content of
    Left PRO -> do
      w <- extractZipperById rng_cp x'tr
      w0 <- parent w
      cp0 <- currentCPDPPP w0 ^? _CPCase
      i0 <- cp0^.complement.specifier.coidx_i
      let cpresult = cp & (complement.specifier.coidx_i .~ Just i0)
      return cpresult
    _ -> do
      rng_cp' <- listToMaybe (cp^..complement.complement.complement.traverse.coidx_content._Right._CompVP_CP)
      w' <- extractZipperById rng_cp' x'tr
      cp' <- currentCPDPPP w' ^? _CPCase
      Left PRO <- return (cp'^.complement.specifier.coidx_content)
      i <- cp'^.complement.specifier.coidx_i
      let cpresult = cp & (complement.specifier.coidx_i .~ Just i)
      return cpresult


bindingPRO :: X'Tree 'PH1 -> X'Tree 'PH1
bindingPRO x'tr = bimap f f x'tr
 where f x = case x^._2 of
               CPCase _ -> (_2._CPCase %~ resolvePRO x'tr) x
               _        -> x




mkX'TreePH1 :: X'Tree 'PH0 -> X'Tree 'PH1
mkX'TreePH1 x'tr = bimap f f x'tr
  where
    f (rng,x) = (rng,mkCPDPPPPH1 x)





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

