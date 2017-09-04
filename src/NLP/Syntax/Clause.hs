{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Monad                          ((<=<),guard,join,void)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Maybe              (MaybeT(..))
import           Control.Monad.Trans.State              (State,execState,get,put)
import           Data.Bifoldable
import           Data.Bitraversable                     (bitraverse)
import           Data.Either                            (partitionEithers)
import           Data.Function                          (on)
import qualified Data.HashMap.Strict               as HM
import           Data.List                              (minimumBy)
import           Data.Maybe                             (fromMaybe,listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid                            (First(..),Last(..),(<>))
import           Data.Text                              (Text)
import qualified Data.Text                         as T
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           Data.Range                             (rangeTree)
import           Lexicon.Type                           (chooseATNode)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (isChunkAs)
--
import           Debug.Trace


maximalProjectionVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
maximalProjectionVP vp = listToMaybe (vp^.vp_words) >>= parent . fst


parentOfVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
parentOfVP vp = parent =<< maximalProjectionVP vp


headVP :: VerbProperty (Zipper (Lemma ': as)) -> Maybe (Zipper (Lemma ': as))
headVP vp = getLast (mconcat (map (Last . Just . fst) (vp^.vp_words)))



splitDP :: Zipper (Lemma ': as) -> Zipper (Lemma ': as)
splitDP z = fromMaybe z $ do
              guard (isChunkAs NP (current z))
              dp <- child1 z
              guard (isChunkAs NP (current dp))
              sbar <- next dp
              guard (isChunkAs SBAR (current sbar))
              return dp


complementsOfVerb :: VerbProperty (Zipper (Lemma ': as)) -> [[Either NTrace (Zipper (Lemma ': as))]]
complementsOfVerb vp = map (\x -> [Right x]) (splitDP <$> (siblingsBy next checkNPSBAR =<< maybeToList (headVP vp)))
  where
    tag = bimap (chunkTag.snd) (posTag.snd) . getRoot
    checkNPSBAR z = case tag z of
                      Left NP    -> True
                      Left SBAR  -> True
                      Left S     -> True
                      Left SBARQ -> True
                      Left SQ    -> True
                      Left _     -> False
                      Right p    -> case isNoun p of
                                      Yes -> True
                                      _   -> False


identifySubject :: N.ClauseTag
                -> Zipper (Lemma ': as)
                -> [Either NTrace (Zipper (Lemma ': as))]
identifySubject tag vp =
  let r = case tag of
            N.SINV -> firstSiblingBy next (isChunkAs NP) vp
            _      -> firstSiblingBy prev (isChunkAs NP) vp
  in case r of
       Nothing -> [Left NULL]
       Just z  -> [Right z]



-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: VerbProperty (Zipper (Lemma ': as))
            -> Maybe (CP (Lemma ': as))
constructCP vprop = do
    vp <- maximalProjectionVP vprop
    tp' <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp'
    let verbp = mkVerbP vp vprop (complementsOfVerb vprop)
    case tptag' of
      N.CL s -> do
        cp' <- parent tp'
        cptag' <- N.convert <$> getchunk cp'
        let subj = identifySubject s vp
        case cptag' of
          N.CL _ -> return $ mkCP (maybe (Left C_NULL) Right (prev tp')) (Just cp') (mkTP (Just tp') subj verbp)
          N.RT   -> return $ mkCP (Left C_NULL) (Just cp') (mkTP (Just tp') subj verbp)
          _      -> -- somewhat problematic case?
                    return (mkCP (Left C_NULL) Nothing (mkTP (Just tp') subj verbp))
      _ -> -- reduced relative clause
           return (mkCP (Left C_WH) (Just vp) (mkTP (Just vp) [Left NULL] verbp))
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



cpRange :: CP xs -> Maybe Range
cpRange cp = (cp^?maximalProjection._Just.to (getRange . current)) <|>
             (cp^?complement.maximalProjection._Just.to (getRange . current)) <|>
             (return (cp^.complement.complement.maximalProjection.to (getRange . current)))



identifyCPHierarchy :: [VerbProperty (Zipper (Lemma ': as))]
                    -> Maybe [Bitree (Range,CP (Lemma ': as)) (Range,CP (Lemma ': as))]
identifyCPHierarchy vps = traverse (bitraverse tofull tofull) rtr
  where cps = mapMaybe ((\cp -> (,) <$> cpRange cp <*> pure cp) <=< constructCP) vps
        cpmap = HM.fromList (map (\x->(x^._1,x)) cps)
        rngs = HM.keys cpmap
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng cpmap







currentCP = snd . getRoot1 . current


whMovement :: BitreeZipper (Range,CP as) (Range,CP as)
           -> State (Bitree (Range,CP as) (Range,CP as)) [Either NTrace (Zipper as)]
whMovement z = do
  let cp = currentCP z
  case cp^.complement.specifier of
    [] -> return []
    xs -> case last xs of
      Left NULL -> do
        let xspro = init xs ++ [Left SilentPRO]
            xsmov = init xs ++ [Left Moved]
        fmap (fromMaybe xspro) . runMaybeT $ do
          -- check subject position for relative pronoun
          z'  <- (MaybeT . return) (prev =<< cp^.maximalProjection)
          return (xsmov ++ [Left WHPRO, Right z'])
      _ -> do
        runMaybeT $ do
          -- check object position for relative pronoun
          z'  <- (MaybeT . return) (prev =<< cp^.maximalProjection)
          let cp' = ((complement.complement.complement) %~ ([Left Moved,Left WHPRO,Right z'] :)) cp
              subtr = case z^.tz_current of
                        PN (rng,cp) ys -> PN (rng,cp') ys
                        PL (rng,cp)    -> PL (rng,cp')
              z'' = (tz_current .~ subtr) z
          lift (put (toBitree z''))
        return xs


-- | This is the final step to resolve silent pronoun. After CP hierarchy structure is identified,
--   silent pronoun should be linked with the subject DP which c-commands the current CP the subject
--   of TP of which is marked as silent pronoun.
--
resolveDP :: forall as.
             Range -> State (Bitree (Range,CP as) (Range,CP as))  [Either NTrace (Zipper as)]
resolveDP rng = do
  tr <- get
  case extractZipperById rng tr of
    Nothing -> return []
    Just z -> do
      let cp = currentCP z
      if either (== C_WH) (isChunkAs WHNP . current) (cp^.headX)
        then whMovement z
        else
          case cp^.complement.specifier of
            [] -> return []
            xs -> case last xs of
                    Left NULL      -> do let xspro = init xs ++ [Left SilentPRO]
                                             xsmov = init xs ++ [Left Moved]
                                         fmap (fromMaybe xspro) . runMaybeT $ do
                                           cp'  <- (MaybeT . return) (currentCP <$> parent z)
                                           rng' <- (MaybeT . return) (cpRange cp')
                                           (++) <$> pure xspro <*> lift (resolveDP rng')
                    Left SilentPRO ->    fmap (fromMaybe xs) . runMaybeT $ do
                                           cp'  <- (MaybeT . return) (currentCP <$> parent z)
                                           rng' <- (MaybeT . return) (cpRange cp')
                                           (++) <$> pure xs <*> lift (resolveDP rng')
                    _              ->    return xs


bindingAnalysis :: Bitree (Range,CP as) (Range,CP as) -> Bitree (Range,CP as) (Range,CP as)
bindingAnalysis cpstr = execState (go rng0) cpstr
   where z0 = either id id . getRoot . mkBitreeZipper [] $ cpstr
         getrng = either fst fst . getRoot . current
         rng0 = (either fst fst . getRoot) cpstr
         go rng = do xs <- resolveDP rng
                     tr <- get
                     case extractZipperById rng tr of
                       Nothing -> return ()
                       Just z -> do
                         let subtr = case z^.tz_current of
                                       PN (rng,cp) ys -> PN (rng,(complement.specifier .~ xs) cp) ys
                                       PL (rng,cp)    -> PL (rng,(complement.specifier .~ xs) cp)
                         let z' = (tz_current .~ subtr) z
                         put (toBitree z')
                         case child1 z' of
                           Just z'' -> go (getrng z'')
                           Nothing ->
                             case next z' of
                               Just z'' -> go (getrng z'')
                               Nothing -> return ()




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


findPAWS :: ClauseTree
         -> VerbProperty (BitreeZipperICP (Lemma ': as))
         -> Maybe [Bitree (Range,CP (Lemma ': as)) (Range,CP (Lemma ': as))]
         -> Maybe (PredArgWorkspace (Lemma ': as) (Either (Range,STag) (Int,POSTag)))
findPAWS tr vp mcpstr = do
  cp <- constructCP vp   -- very inefficient. but for testing.
  rng <- cpRange cp
  cpstr <- mcpstr
  cp' <- snd . getRoot1 . current <$> ((getFirst . foldMap (First . extractZipperById rng)) cpstr)
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
