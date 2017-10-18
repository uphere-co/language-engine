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
import           Control.Monad                          ((<=<),guard,void)
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
import           Data.ListZipper
import           Data.Range                             (rangeTree)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Voice(..))
import           NLP.Type.TagPos                        (TagPos(..),TokIdx)
--
import           NLP.Syntax.Noun                        (splitDP)
import           NLP.Syntax.Preposition                 (checkEmptyPrep)
import           NLP.Syntax.Type                        (ClauseTree,ClauseTreeZipper,SBARType(..),STag(..),MarkType(..),PredArgWorkspace(..))
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs,isPOSAs,mergeLeftELZ,mergeRightELZ)
--
import qualified Data.Text as T
import           NLP.Syntax.Format.Internal
import Debug.Trace


hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return


maximalProjectionVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
maximalProjectionVP vp = listToMaybe (vp^.vp_words) >>= parent . fst


parentOfVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
parentOfVP vp = parent =<< maximalProjectionVP vp


headVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
headVP vp = getLast (mconcat (map (Last . Just . fst) (vp^.vp_words)))


complementsOfVerb :: [TagPos TokIdx MarkType]
                  -> VerbProperty (Zipper (Lemma ': as))
                  -> [TraceChain (CompVP (Lemma ': as))]
complementsOfVerb tagged vprop =
  let nextNotComma z = do n <- next z
                          guard (not (isPOSAs M_COMMA (current n)))
                          return n
      cs = map xform (siblingsBy (nextNotComma) checkNPSBAR =<< maybeToList (headVP vprop))
  in case vprop^.vp_voice of
       Active -> cs
       Passive -> TraceChain (Left (singletonLZ Moved)) Nothing : cs
  where
    xform_dp = TraceChain (Right []) . Just . checkEmptyPrep tagged . splitDP tagged
    xform_cp = TraceChain (Right []) . Just . CompVP_Unresolved
    xform z = case tag (current z) of
                Left NP    -> xform_dp z
                Left _     -> xform_cp z
                Right p    -> if isNoun p == Yes then xform_dp z else xform_cp z

    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkNPSBAR z = case tag z of
                      Left NP    -> True
                      Left SBAR  -> True
                      Left S     -> True
                      Left SBARQ -> True
                      Left SQ    -> True
                      Left _     -> False
                      Right p    -> isNoun p == Yes


allAdjunctCPOfVerb vprop =
    let mcomma = firstSiblingBy next (isPOSAs M_COMMA) =<< headVP vprop
    in case mcomma of
         Nothing -> []
         Just comma -> siblingsBy next checkS comma
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkS z = case tag z of
                    Left SBAR  -> True
                    Left S     -> True
                    Left SBARQ -> True
                    Left SQ    -> True
                    _          -> False




identifySubject :: [TagPos TokIdx MarkType]
                -> N.ClauseTag
                -> Zipper (Lemma ': as)   -- ^ Verb maximal projection
                -- -> VerbProperty (Zipper (Lemma ': as))
                -- -> [TraceChain (CompVP (Lemma ': as))]
                -> TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as)))
identifySubject tagged tag vp = maybe nul smp r
  where
    r = case tag of
          N.SINV -> firstSiblingBy next (isChunkAs NP) vp          -- this should be refined.
          _      -> firstSiblingBy prev (isChunkAs NP) vp          -- this should be refined.
    nul = TraceChain (Left (singletonLZ NULL)) Nothing
    smp z = TraceChain (Right [])(Just (Right (splitDP tagged z))) -- for the time being, CP subject is not supported





--
-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: [TagPos TokIdx MarkType]
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as),[Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))])
constructCP tagged vprop = do
    vp <- maximalProjectionVP vprop
    tp <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp
    let comps = complementsOfVerb tagged vprop
        adjs  = allAdjunctCPOfVerb vprop
        comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToEither)
        verbp = mkVerbP vp vprop comps
        nullsubj = TraceChain (Left (singletonLZ NULL)) Nothing
    case tptag' of
      N.CL s -> do
        cp' <- parent tp
        cptag' <- N.convert <$> getchunk cp'
        let subj = identifySubject tagged s vp -- vprop comps
            subj_dps = maybeToList (subj ^? trResolved._Just)
            dps = (subj_dps++comps_dps)
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
           return (mkCP C_PHI vp (Just SpecCP_WHPHI) adjs (mkTP vp nullsubj verbp),comps_dps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



cpRange :: CP xs -> Range
cpRange cp = cp^.maximalProjection.to (getRange . current)


hierarchyBits :: (CP as, [DetP as]) -> Maybe [(Range, (Range, CPDP as))]
hierarchyBits (cp,zs) = do
  let rng = cpRange cp
  let cpbit = (rng,(rng,CPCase cp))

  let f z = let rng' = z^.maximalProjection.to current.to getRange in (rng',(rng',DPCase z))
  return (cpbit:map f zs)



identifyCPHierarchy :: [TagPos TokIdx MarkType]
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> [X'Tree (Lemma ': as)]
identifyCPHierarchy tagged vps = fromMaybe [] (traverse (bitraverse tofull tofull) rtr)
  where cpmap = (HM.fromList . concat . mapMaybe (hierarchyBits . (_2 %~ rights) <=< constructCP tagged)) vps
        rngs = HM.keys cpmap
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng cpmap


currentCPDP :: X'Zipper as -> CPDP as
currentCPDP = snd . getRoot1 . current


adjustX'Tree :: ((Range, CPDP as) -> (Range, CPDP as))
               -> X'Zipper as -> DetP as -> MaybeT (State (X'Tree as)) ()
adjustX'Tree f w z = do
  let dprng = z ^. maximalProjection.to current.to getRange
      -- adjust CPDP hierarchy by modifier relation.
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



retrieveZCP :: Range -> MaybeT (State (X'Tree (Lemma ': as))) (X'Zipper (Lemma ': as),CP (Lemma ': as))
retrieveZCP rng = do
  tr <- lift get
  z <- hoistMaybe (extractZipperById rng tr)
  cp <- hoistMaybe (currentCPDP z ^? _CPCase)
  return (z,cp)




whMovement :: [TagPos TokIdx MarkType]
           -> X'Zipper (Lemma ': as)
           -> State (X'Tree (Lemma ': as)) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
whMovement tagged w =
  -- letter z denotes zipper for PennTree, w denotes zipper for Bitree (Range,CPDP as) (Range,CPDP as)
  case currentCPDP w of
    CPCase cp -> do
      let spec = cp^.complement.specifier
      -- whMovement process starts with cheking trace in subject
      case spec^.trChain of
        Left (LZ ps c ns) ->
          -- with trace in subject
          -- check subject for relative pronoun
          case c of
            NULL -> do
              -- ignore ns.
              let xspro = LZ ps SilentPRO []
              fmap (fromMaybe (TraceChain (Left xspro) Nothing)) . runMaybeT $ do
                -- check subject position for relative pronoun
                z' <- splitDP tagged <$> hoistMaybe (prev (cp^.maximalProjection))
                adjustX'Tree id w z'
                return (TraceChain (Left (LZ ps Moved [WHPRO])) (Just (Right z')))
            _    -> return spec
        Right _ -> do
          -- without trace in subject
          -- check object for relative pronoun
          runMaybeT $ do
            z'  <- splitDP tagged <$> hoistMaybe (prev (cp^.maximalProjection))
            let -- adjust function for complement with relative pronoun resolution
                rf0 = _2._CPCase.complement.complement.complement
                       %~ (TraceChain (Left (LZ [] Moved [WHPRO])) (Just (CompVP_DP z')) :)
            -- adjust CPDP hierarchy by modifier relation.
            adjustX'Tree rf0 w z'
            return ()
          return spec
    _  -> return emptyTraceChain


resolveSilentPRO :: [TagPos TokIdx MarkType]
                 -> (X'Zipper (Lemma ': as),CP (Lemma ': as))
                 -> MaybeT (State (X'Tree (Lemma ': as))) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveSilentPRO tagged (z,cp) = do
  let spec = cp^.complement.specifier
  case spec^.trChain of
    Right _ -> return spec
    Left (xs@(LZ _ c _)) -> case c of
      NULL      -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                        let rng' = cpRange cp'
                        TraceChain exs' x' <- lift (resolveDP tagged rng')
                        return (TraceChain (mergeLeftELZ (Left (replaceLZ SilentPRO xs)) exs') x'))
                    <|>
                    return (TraceChain (Left (replaceLZ SilentPRO xs)) Nothing))
      SilentPRO -> ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                        let rng' = cpRange cp'
                        TraceChain exs' x' <- lift (resolveDP tagged rng')
                        let xs' = either lzToList id exs'
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
  (z,cp) <- retrieveZCP rng
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
              z' = replaceFocusItem rf rf z
          lift (put (toBitree z'))
          return (TraceChain (mergeRightELZ (c^.trChain) (spec^.trChain)) (spec^.trResolved))


--
-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
resolveDP :: [TagPos TokIdx MarkType]
          -> Range
          -> State (X'Tree (Lemma ': as)) (TraceChain (Either (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveDP tagged rng = fmap (fromMaybe emptyTraceChain) . runMaybeT $ do
  (z,cp) <- retrieveZCP rng
  if is _Just (cp^.specifier)  -- relative clause
    then resolveVPComp rng =<< lift (whMovement tagged z)
    else resolveVPComp rng =<< resolveSilentPRO tagged (z,cp)



resolveCP :: forall as. X'Tree (Lemma ': as) -> X'Tree (Lemma ': as)
resolveCP xtr = rewriteTree action xtr
  where
    action rng = do z <- hoistMaybe . extractZipperById rng =<< lift get
                    (replace z <|> return z)

    replace :: X'Zipper (Lemma ': as) -> MaybeT (State (X'Tree (Lemma ': as))) (X'Zipper (Lemma ': as))
    replace z = do cp <- hoistMaybe (z ^? to current.to getRoot1._2._CPCase)
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





bindingSpec rng spec = do
  z <- hoistMaybe . extractZipperById rng =<< lift get
  let rf = _2._CPCase.complement.specifier .~ spec
      z' = replaceFocusItem rf rf z
  lift (put (toBitree z'))
  return z'


nextLZ (LZ ps c (n:ns)) = Just (LZ (c:ps) n ns)
nextLZ _                = Nothing


-- consider passive case only now for the time being.
connectRaisedDP :: Range -> MaybeT (State (X'Tree (Lemma ': as))) (X'Zipper (Lemma ': as))
connectRaisedDP rng = do
  (z,cp) <- retrieveZCP rng
  guard (cp ^. complement.complement.headX.vp_voice == Passive)
  c1:c2:[] <- return (cp^.complement.complement.complement)
  rng1 <- hoistMaybe (c1^?trResolved._Just._CompVP_DP.to headRange)
  cp' <- hoistMaybe (c2^?trResolved._Just._CompVP_CP)
  let rng' = cpRange cp'
  rng_dp <- hoistMaybe (cp'^?complement.specifier.trResolved._Just._Right.to headRange)
  if rng1 == rng_dp
    then do
      let rf = (_2._CPCase.complement.specifier .~ emptyTraceChain)
             . (_2._CPCase.complement.complement.complement .~ [c2])
          z' = replaceFocusItem rf rf z
      lift (put (toBitree z'))
      return z'
    else
      return z


-- I think we should change the name of these bindingAnalysis.. functions.

--
-- | This is the final step to bind inter-clause trace chain
--
bindingAnalysis :: [TagPos TokIdx MarkType] -> X'Tree (Lemma ': as) -> X'Tree (Lemma ': as)
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



predicateArgWS :: CP xs
               -> ClauseTreeZipper
               -> PredArgWorkspace xs (Either (Range,STag) (Int,POSTag))
predicateArgWS cp z =
  PAWS { _pa_CP = cp
       , _pa_candidate_args = case child1 z of
                                Nothing -> []
                                Just z' -> map extractArg (z':iterateMaybe next z')
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


findPAWS :: [TagPos TokIdx MarkType]
         -> ClauseTree
         -> VerbProperty (BitreeZipperICP (Lemma ': as))
         -> [X'Tree (Lemma ': as)]
         -> Maybe (PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag)))
findPAWS tagged tr vp cpstr = do
  cp <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
  let rng = cpRange cp
  cp' <- (^? _CPCase) . currentCPDP =<< ((getFirst . foldMap (First . extractZipperById rng)) cpstr)
  predicateArgWS cp' <$> findVerb (vp^.vp_index) tr



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
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_PP _ _,_)))   ->  [x1,x2]
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_SBAR _,_))) ->  [x1,x2]
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_CL _,_)))   ->  [x1,x2]
    _ -> [x]
promote_PP_CP_from_NP x = [x]



clauseStructure :: [VerbProperty (Zipper '[Lemma])]
                -> PennTreeIdxG N.CombinedTag (POSTag,Text)
                -> ClauseTree
clauseStructure _vps (PL (i,pt)) = PL (Right (i,pt))
clauseStructure vps  (PN (rng,tag) xs)
  = let ys = map (clauseStructure vps) xs
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
                                        PN (_,(N.CL N.S)) _lst : _ -> S_PP t True
                                        _                          -> S_PP t False
                           in case tail ys of
                                [] -> PL (Left (rng,(prep,lvl)))
                                _  -> PN (rng,(prep,lvl)) (tail ys)
                         -- prepositional case 2
                         PL (_,(TO,t)):os ->
                           -- we need to rewrite this whole functions using zipper later.
                           let prep = case os of
                                        PN (_,(N.CL N.S)) _lst : _ -> S_PP t True
                                        _                          -> S_PP t False
                           in case tail ys of
                                [] -> PL (Left (rng,(prep,lvl)))
                                _ -> PN (rng,(prep,lvl)) (tail ys)
                         -- non-prepositional case
                         _ -> PL (Left (rng,(S_PP "" False,lvl)))
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
         S_PP    _ _ -> PL (Left (rng,(p,lvl)))
         S_OTHER _   -> PL (Left (rng,(p,lvl)))
         _           -> PN (rng,(p,lvl)) (map cutOutLevel0 xs)
  else PN (rng,(p,lvl)) (map cutOutLevel0 xs)
