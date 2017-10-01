{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Lens.Extras                    (is)
import           Control.Monad                          ((<=<),void)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,execState,get,put)
import           Data.Bifoldable
import           Data.Bitraversable                     (bitraverse)
import           Data.Either                            (partitionEithers,rights)
import qualified Data.HashMap.Strict               as HM
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (First(..),Last(..),(<>))
import           Data.Text                              (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           Data.Range                             (rangeTree)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.TagPos                        (TagPos(..),TokIdx)
--
import           NLP.Syntax.Noun                        (splitDP)
import           NLP.Syntax.Preposition                 (checkEmptyPrep)
import           NLP.Syntax.Type                        (ClauseTree,ClauseTreeZipper,SBARType(..),STag(..),MarkType(..),PredArgWorkspace(..))
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs)
--


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
complementsOfVerb tagged vp = {- map (\x -> TraceChain [] (Just (checkEmptyPrep tagged x)))
                                  (splitDP tagged <$> -}

                              map xform (siblingsBy next checkNPSBAR =<< maybeToList (headVP vp))
  where
    xform_dp = TraceChain [] . Just . checkEmptyPrep tagged . splitDP tagged
    xform_cp = TraceChain [] . Just . CompVP_Unresolved
    xform z = case tag (current z) of
                Left NP    -> xform_dp z
                Left _     -> xform_cp z
                {- Left S     -> xform_cp z
                Left SBARQ -> xform_cp z
                Left SQ    -> xform_cp z
                Left _     -> -- False -}
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


identifySubject :: [TagPos TokIdx MarkType]
                -> N.ClauseTag
                -> Zipper (Lemma ': as)
                -> TraceChain (Either {- (CP (Lemma ': as)) -} (Zipper (Lemma ': as)) (DetP (Lemma ': as)))   -- now support clause subject!
identifySubject tagged tag vp =
  let r = case tag of
            N.SINV -> firstSiblingBy next (isChunkAs NP) vp
            _      -> firstSiblingBy prev (isChunkAs NP) vp
  in case r of
       Nothing -> TraceChain [NULL] Nothing
       Just z  -> TraceChain []     (Just (Right (splitDP tagged z)))   -- for the time being, CP subject is not supported




-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: [TagPos TokIdx MarkType]
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as),[Either {- (CP (Lemma ':as )) -} (Zipper (Lemma ': as)) (DetP (Lemma ': as))])
constructCP tagged vprop = do
    vp <- maximalProjectionVP vprop
    tp <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp
    let comps = complementsOfVerb tagged vprop
        comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to compVPToEither)
        verbp = mkVerbP vp vprop comps
        nullsubj = TraceChain [NULL] Nothing
    case tptag' of
      N.CL s -> do
        cp' <- parent tp
        cptag' <- N.convert <$> getchunk cp'
        let subj = identifySubject tagged s vp
            subj_dps = maybeToList (subj ^? trResolved._Just)
            dps = (subj_dps++comps_dps)
        case cptag' of
          N.CL N.SBAR ->
            let (cphead,cpspec) = case prev tp of
                                    Nothing -> (C_PHI,Nothing)
                                    Just z -> if (isChunkAs WHNP (current z))
                                              then (C_PHI,Just (SpecCP_WH z))
                                              else (C_WORD z,Nothing)
            in return (mkCP cphead (Just cp') cpspec (mkTP (Just tp) subj verbp),dps)
          N.CL _ ->
            return (mkCP C_PHI (Just tp) Nothing (mkTP (Just tp) subj verbp),dps)
          N.RT   ->
            return (mkCP C_PHI (Just cp') Nothing (mkTP (Just tp) subj verbp),dps)
          _      -> -- somewhat problematic case?
            return (mkCP C_PHI Nothing Nothing (mkTP (Just tp) subj verbp),dps)
      _ -> -- reduced relative clause
           return (mkCP C_PHI (Just vp) (Just SpecCP_WHPHI) (mkTP (Just vp) nullsubj verbp),comps_dps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



cpRange :: CP xs -> Maybe Range
cpRange cp = (cp^?maximalProjection._Just.to (getRange . current)) <|>
             (cp^?complement.maximalProjection._Just.to (getRange . current)) <|>
             (return (cp^.complement.complement.maximalProjection.to (getRange . current)))


hierarchyBits :: (CP as, [DetP as]) -> Maybe [(Range, (Range, CPDP as))]
hierarchyBits (cp,zs) = do
  rng <- cpRange cp
  let cpbit = (rng,(rng,CPCase cp))

  let f z = let rng' = z^.maximalProjection.to current.to getRange in (rng',(rng',DPCase z))
  return (cpbit:map f zs)



identifyCPHierarchy :: [TagPos TokIdx MarkType]
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> [Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))]
identifyCPHierarchy tagged vps = fromMaybe [] (traverse (bitraverse tofull tofull) rtr)
  where cpmap = (HM.fromList . concat . mapMaybe (hierarchyBits . (_2 %~ rights) <=< constructCP tagged)) vps
        rngs = HM.keys cpmap
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng cpmap


currentCPDP :: BitreeZipper (Range,CPDP as) (Range,CPDP as) -> CPDP as
currentCPDP = snd . getRoot1 . current


adjustXBarTree :: ((Range, CPDP as) -> (Range, CPDP as))
               -> BitreeZipper (Range, CPDP as) (Range, CPDP as)
               -> DetP as
               -> MaybeT (State (Bitree (Range,CPDP as) (Range, CPDP as))) ()
adjustXBarTree f w z = do
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



whMovement :: [TagPos TokIdx MarkType]
           -> BitreeZipper (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))
           -> State (Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as)))
                (TraceChain (Either {- (CP (Lemma ': as)) -} (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
whMovement tagged w =
  -- letter z denotes zipper for PennTree, w denotes zipper for Bitree (Range,CPDP as) (Range,CPDP as)
  case currentCPDP w of
    CPCase cp -> do
      let spec = cp^.complement.specifier
      -- whMovement process starts with cheking trace in subject
      let xs = spec^.trChain
      if (not . null) xs
        then
          -- with trace in subject
          -- check subject for relative pronoun
          case last xs of
            NULL -> do
              let xspro = init xs ++ [SilentPRO]
                  xsmov = init xs ++ [Moved]
              fmap (fromMaybe (TraceChain xspro Nothing)) . runMaybeT $ do
                -- check subject position for relative pronoun
                z' <- splitDP tagged <$> hoistMaybe (prev =<< cp^.maximalProjection)
                adjustXBarTree id w z'
                return (TraceChain (xsmov ++ [WHPRO]) (Just (Right z')))
            _    -> return spec
        else do
          -- without trace in subject
          -- check object for relative pronoun
          runMaybeT $ do
            z'  <- splitDP tagged <$> hoistMaybe (prev =<< cp^.maximalProjection)
            let -- adjust function for complement with relative pronoun resolution
                rf0 = _2._CPCase.complement.complement.complement %~ (TraceChain [Moved,WHPRO] (Just (CompVP_DP z')) :)
            -- adjust CPDP hierarchy by modifier relation.
            adjustXBarTree rf0 w z'
            return ()
          return spec
    _  -> return emptyTraceChain


-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
resolveDP :: [TagPos TokIdx MarkType]
          -> Range
          -> State (Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as)))
               (TraceChain (Either {- (CP (Lemma ': as)) -} (Zipper (Lemma ': as)) (DetP (Lemma ': as))))
resolveDP tagged rng = fmap (fromMaybe emptyTraceChain) . runMaybeT $ do
  tr <- lift get
  z <- hoistMaybe (extractZipperById rng tr)
  cp <- hoistMaybe (currentCPDP z ^? _CPCase)
  if is _Just (cp^.specifier)  -- relative clause
    then lift (whMovement tagged z)
    else do
      let spec = cp^.complement.specifier
      case spec^.trChain of
        [] -> return spec
        xs -> case last xs of
                NULL      -> do let xspro = init xs ++ [SilentPRO]
                                ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                                     rng' <- hoistMaybe (cpRange cp')
                                     TraceChain xs' x' <- lift (resolveDP tagged rng')
                                     return (TraceChain (xspro <> xs') x'))
                                 <|>
                                 return (TraceChain xspro Nothing))

                SilentPRO ->    ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                                     rng' <- hoistMaybe (cpRange cp')
                                     TraceChain xs' x' <- lift (resolveDP tagged rng')
                                     return (TraceChain (xs <> xs') x'))
                                 <|>
                                 return (TraceChain xs Nothing))

                _         ->    return spec



resolveCP :: forall as.
             Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))
          -> Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))
resolveCP cpstr = execState (go rng0) cpstr
  where
    getrng = fst . getRoot1 . current
    rng0 = (fst . getRoot1) cpstr
    go :: Range
       -> State (Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))) ()
    go rng = do tr <- get
                void . runMaybeT $ do
                  z <- hoistMaybe (extractZipperById rng tr)
                  z' <- (replace z <|> return z)
                  ((hoistMaybe (child1 z') >>= \z'' -> lift (go (getrng z'')))
                   <|>
                   (hoistMaybe (next z') >>= \z'' -> lift (go (getrng z''))))

    replace :: BitreeZipper (Range, CPDP (Lemma ': as)) (Range, CPDP (Lemma ': as))
            -> MaybeT (State (Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as)))) (BitreeZipper (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as)))
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
                   -- let rf = _2._CPCase.complement.complement.complement .~ xs'
                   let z' = replaceFocusItem (_2._CPCase.complement.complement.complement .~ xs') (_2._CPCase.complement.complement.complement .~ xs') z
                   lift (put (toBitree z'))
                   return z'



bindingAnalysis :: [TagPos TokIdx MarkType]
                -> Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))
                -> Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))
bindingAnalysis tagged cpstr = execState (go rng0) cpstr
   where
     getrng = either fst fst . getRoot . current
     rng0 = (either fst fst . getRoot) cpstr
     go rng = do xs <- resolveDP tagged rng
                 tr <- get
                 void . runMaybeT $ do
                   z <- hoistMaybe (extractZipperById rng tr)
                   let z' = replaceFocusItem (_2._CPCase.complement.specifier .~ xs) (_2._CPCase.complement.specifier .~ xs) z
                   lift (put (toBitree z'))
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
         -> [Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))]
         -> Maybe (PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag)))
findPAWS tagged tr vp cpstr = do
  cp <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
  rng <- cpRange cp
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
