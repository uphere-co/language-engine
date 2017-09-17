{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Monad                          ((<=<),void)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,execState,get,put)
import           Data.Bifoldable
import           Data.Bitraversable                     (bitraverse)
import           Data.Either                            (partitionEithers)
import qualified Data.HashMap.Strict               as HM
import           Data.List                              (find,mapAccumL,inits)
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
import           NLP.Syntax.Preposition                 (checkEmptyPrep,hasEmptyPreposition)
import           NLP.Syntax.Type                        (ClauseTree,ClauseTreeZipper,SBARType(..),STag(..),MarkType(..),PredArgWorkspace(..))
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs)
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


complementsOfVerb :: [TagPos TokIdx MarkType]
                  -> VerbProperty (Zipper (Lemma ': as))
                  -> [TraceChain (DPorPP (Zipper (Lemma ': as)))]
complementsOfVerb tagged vp = map (\x -> TraceChain [] (Just (checkEmptyPrep tagged x)))
                                  ((\z -> fromMaybe z (splitDP z)) <$>
                                   (siblingsBy next checkNPSBAR =<< maybeToList (headVP vp)))
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkNPSBAR z = case tag z of
                      Left NP    -> True
                      Left SBAR  -> True
                      Left S     -> True
                      Left SBARQ -> True
                      Left SQ    -> True
                      Left _     -> False
                      Right p    -> isNoun p == Yes


identifySubject :: N.ClauseTag
                -> Zipper (Lemma ': as)
                -> TraceChain (Zipper (Lemma ': as))
identifySubject tag vp =
  let r = case tag of
            N.SINV -> firstSiblingBy next (isChunkAs NP) vp
            _      -> firstSiblingBy prev (isChunkAs NP) vp
  in case r of
       Nothing -> TraceChain [NULL] Nothing
       Just z  -> TraceChain []     (Just z)




-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: [TagPos TokIdx MarkType]
            -> VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as),[Zipper (Lemma ': as)])
constructCP tagged vprop = do
    vp <- maximalProjectionVP vprop
    tp <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp
    let comps = complementsOfVerb tagged vprop
        comps_dps = comps & mapMaybe (\x -> x ^? trResolved._Just.to removeDPorPP)
        verbp = mkVerbP vp vprop comps
        nullsubj = TraceChain [NULL] Nothing
    case tptag' of
      N.CL s -> do
        cp' <- parent tp
        cptag' <- N.convert <$> getchunk cp'
        let subj = identifySubject s vp
            subj_dps = maybeToList (subj ^? trResolved._Just)
            dps = (subj_dps++comps_dps)
        case cptag' of
          N.CL N.SBAR ->
            return (mkCP (maybe (Left C_NULL) Right (prev tp)) (Just cp') (mkTP (Just tp) subj verbp),dps)
          N.CL _ ->
            return (mkCP (Left C_NULL) (Just tp) (mkTP (Just tp) subj verbp),dps)
          N.RT   ->
            return (mkCP (Left C_NULL) (Just cp') (mkTP (Just tp) subj verbp),dps)
          _      -> -- somewhat problematic case?
            return (mkCP (Left C_NULL) Nothing (mkTP (Just tp) subj verbp),dps)
      _ -> -- reduced relative clause
           return (mkCP (Left C_WH) (Just vp) (mkTP (Just vp) nullsubj verbp),comps_dps)
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



cpRange :: CP xs -> Maybe Range
cpRange cp = (cp^?maximalProjection._Just.to (getRange . current)) <|>
             (cp^?complement.maximalProjection._Just.to (getRange . current)) <|>
             (return (cp^.complement.complement.maximalProjection.to (getRange . current)))


hierarchyBits (cp,zs) = do
  rng <- cpRange cp
  let cpbit = (rng,(rng,CPCase cp))

  let f z = let rng' = (getRange . current) z in (rng',(rng',DPCase z))
  return (cpbit:map f zs)



identifyCPHierarchy :: [TagPos TokIdx MarkType]
                    -> [VerbProperty (Zipper (Lemma ': as))]
                    -> Maybe [Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))]
identifyCPHierarchy tagged vps = traverse (bitraverse tofull tofull) rtr
  where cpmap = (HM.fromList . concat . mapMaybe (hierarchyBits <=< constructCP tagged)) vps
        rngs = HM.keys cpmap
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng cpmap


currentCPDP :: BitreeZipper (Range,CPDP as) (Range,CPDP as) -> CPDP as
currentCPDP = snd . getRoot1 . current



whMovement :: BitreeZipper (Range,CPDP as) (Range,CPDP as)
           -> State (Bitree (Range,CPDP as) (Range,CPDP as)) (TraceChain (Zipper as))
whMovement w =
  -- letter z denotes zipper for PennTree, w denotes zipper for Bitree (Range,CPDP as) (Range,CPDP as)
  case currentCPDP w of
    DPCase _  -> return emptyTraceChain
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
                z'  <- hoistMaybe (prev =<< cp^.maximalProjection)
                let dprng = getRange (current z')
                    -- adjust CPDP hierarchy by modifier relation.
                case extractZipperById dprng (toBitree w) of
                  Nothing -> do let newtr (PN y ys) = PN (dprng,DPCase z') [PN y ys]
                                    newtr (PL y)    = PN (dprng,DPCase z') [PL y]
                                    w'' = replaceTree newtr w
                                lift (put (toBitree w''))

                  Just _  -> do let otr = current w
                                lift . put =<< hoistMaybe (remove w)
                                w' <- MaybeT (extractZipperById dprng <$> get)
                                let newtr (PN y ys) = PN y (ys ++ [otr])
                                    newtr (PL y)    = PN y [otr]
                                    w'' =replaceTree newtr w'
                                lift (put (toBitree w''))
                return (TraceChain (xsmov ++ [WHPRO]) (Just z'))
            _    -> return spec -- do
        else do
          -- without trace in subject
          -- check object for relative pronoun
          runMaybeT $ do
            z'  <- hoistMaybe (prev =<< cp^.maximalProjection)
            let -- adjust function for complement with relative pronoun resolution
                rf0 = _2._CPCase.complement.complement.complement %~ (TraceChain [Moved,WHPRO] (Just (DP z')) :)
                dprng = getRange (current z')
                -- adjust CPDP hierarchy by modifier relation.
                newtr (PN y ys) = PN (dprng,DPCase z') [PN (rf0 y) ys]
                newtr (PL y)    = PN (dprng,DPCase z') [PL (rf0 y)]
                w'' = replaceTree newtr w
            lift (put (toBitree w''))
          return spec


-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
resolveDP :: forall as. Range -> State (Bitree (Range,CPDP as) (Range,CPDP as)) (TraceChain (Zipper as))
resolveDP rng = fmap (fromMaybe emptyTraceChain) . runMaybeT $ do
  tr <- lift get
  z <- hoistMaybe (extractZipperById rng tr)
  cp <- hoistMaybe (currentCPDP z ^? _CPCase)
  if either (== C_WH) (isChunkAs WHNP . current) (cp^.headX)
    then lift (whMovement z)
    else do
      let spec = cp^.complement.specifier
      case spec^.trChain of
        [] -> return spec
        xs -> case last xs of
                NULL      -> do let xspro = init xs ++ [SilentPRO]
                                ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                                     rng' <- hoistMaybe (cpRange cp')
                                     TraceChain xs' x' <- lift (resolveDP rng')
                                     return (TraceChain (xspro <> xs') x'))
                                 <|>
                                 return (TraceChain xspro Nothing))

                SilentPRO ->    ((do cp'  <- hoistMaybe ((^? _CPCase) . currentCPDP =<< parent z)
                                     rng' <- hoistMaybe (cpRange cp')
                                     TraceChain xs' x' <- lift (resolveDP rng')
                                     return (TraceChain (xs <> xs') x'))
                                 <|>
                                 return (TraceChain xs Nothing))

                _         ->    return spec


bindingAnalysis :: Bitree (Range,CPDP as) (Range,CPDP as) -> Bitree (Range,CPDP as) (Range,CPDP as)
bindingAnalysis cpstr = execState (go rng0) cpstr
   where getrng = either fst fst . getRoot . current
         rng0 = (either fst fst . getRoot) cpstr
         go rng = do xs <- resolveDP rng
                     tr <- get
                     void . runMaybeT $ do
                       z <- hoistMaybe (extractZipperById rng tr)
                       let z' = replaceItem (_2._CPCase.complement.specifier .~ xs) (_2._CPCase.complement.specifier .~ xs) z
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
         -> Maybe [Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))]
         -> Maybe (PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag)))
findPAWS tagged tr vp mcpstr = do
  cp <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
  rng <- cpRange cp
  cpstr <- mcpstr
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
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_PP _,_)))   ->  [x1,x2]
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
                         PL (_,(IN,t)):_ ->
                           case tail ys of
                             [] -> PL (Left (rng,(S_PP t,lvl)))
                             _  -> PN (rng,(S_PP t,lvl)) (tail ys)
                         PL (_,(TO,t)):_ ->
                           case tail ys of
                             [] -> PL (Left (rng,(S_PP t,lvl)))
                             _  -> PN (rng,(S_PP t,lvl)) (tail ys)
                         _               -> PL (Left (rng,(S_PP "",lvl)))
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
         S_PP    _ -> PL (Left (rng,(p,lvl)))
         S_OTHER _ -> PL (Left (rng,(p,lvl)))
         _         -> PN (rng,(p,lvl)) (map cutOutLevel0 xs)
  else PN (rng,(p,lvl)) (map cutOutLevel0 xs)
