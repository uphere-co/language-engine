{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module NLP.Syntax.Clause where

import           Control.Applicative                    ((<|>))
import           Control.Lens
import           Control.Monad                          ((<=<))
import           Data.Bifoldable
import           Data.Bitraversable                     (bitraverse)
import           Data.Either                            (partitionEithers)
import           Data.Function                          (on)
import qualified Data.HashMap.Strict               as HM
import           Data.IntMap                            (IntMap)
import           Data.List                              (find,inits,mapAccumL,minimumBy)
import           Data.Maybe                             (listToMaybe,mapMaybe,maybeToList)
import           Data.Monoid
import           Data.Text                              (Text)
import qualified Data.Text                         as T
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Range                             (isInsideR,rangeTree)
import           Lexicon.Type                           (ATNode(..),chooseATNode)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Type
import           NLP.Syntax.Util
import           NLP.Syntax.Verb


maximalProjectionVP :: VerbProperty (BitreeZipperICP '[Lemma]) -> Maybe (BitreeZipperICP '[Lemma])
maximalProjectionVP vp = listToMaybe (vp^.vp_words) >>= parent . fst


parentOfVP :: VerbProperty (BitreeZipperICP '[Lemma]) -> Maybe (BitreeZipperICP '[Lemma])
parentOfVP vp = parent =<< maximalProjectionVP vp


headVP :: VerbProperty (BitreeZipperICP '[Lemma]) -> Maybe (BitreeZipperICP '[Lemma])
headVP vp = getLast (mconcat (map (Last . Just . fst) (vp^.vp_words)))


complementsOfVerb :: VerbProperty (BitreeZipperICP '[Lemma]) -> [BitreeZipperICP '[Lemma]]
complementsOfVerb vp = maybeToList (headVP vp) >>= siblingsBy next checkNPSBAR
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
                -> BitreeZipperICP '[Lemma]
                -> Maybe (ATNode (DP (BitreeZipperICP '[Lemma])))
identifySubject tag vp =
  let r = case tag of
            N.SINV -> firstSiblingBy next (isChunkAs NP) vp
            _      -> firstSiblingBy prev (isChunkAs NP) vp
  in case r of
       Nothing -> Just (SimpleNode SilentPRO)
       Just z  -> Just (SimpleNode (RExp z))


-- | Constructing CP umbrella and all of its ingrediant.
--
constructCP :: VerbProperty (BitreeZipperICP '[Lemma]) -> Maybe CP
constructCP vprop = do
    vp <- maximalProjectionVP vprop
    tp' <- parentOfVP vprop
    tptag' <- N.convert <$> getchunk tp'
    let verbp = VerbP vp vprop (complementsOfVerb vprop)
    case tptag' of
      N.CL s -> do
        cp' <- parent tp'
        cptag' <- N.convert <$> getchunk cp'
        let subj = identifySubject s vp
        case cptag' of
          N.CL _ -> return $ CP (Just cp')
                                (prev tp')
                                (TP (Just tp') subj verbp)
          N.RT   -> return $ CP (Just cp')
                                Nothing
                                (TP (Just tp') subj verbp)
          _      -> return (CP Nothing Nothing (TP (Just tp') subj verbp))  -- somewhat problematic case?
      _ -> return (CP Nothing Nothing (TP Nothing Nothing verbp))           -- reduced relative clause
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current




cpRange cp = (cp^?cp_maximal_projection._Just.to (getRange . current)) <|>
             (cp^?cp_TP.tp_maximal_projection._Just.to (getRange . current))



identifyCPHierarchy :: [VerbProperty (BitreeZipperICP '[Lemma])]
                    -> Maybe [Bitree (Range,CP) (Range,CP)]
identifyCPHierarchy vps = traverse (bitraverse tofull tofull) rtr
  where cps = mapMaybe ((\cp -> (,) <$> cpRange cp <*> pure cp) <=< constructCP) vps 
        cpmap = HM.fromList (map (\x->(x^._1,x)) cps)
        rngs = HM.keys cpmap
        rtr = rangeTree rngs
        tofull rng = HM.lookup rng cpmap


getRoot1 :: Bitree a a -> a
getRoot1 = either id id . getRoot


mkCPZipper :: Range -> Bitree (Range,CP) (Range,CP) -> Maybe (BitreeZipper (Range,CP) (Range,CP))
mkCPZipper rng tr = find (\z -> fst (getRoot1 (current z)) == rng) $ biList (mkBitreeZipper [] tr)


resolvePRO :: BitreeZipper (Range,CP) (Range,CP) -> Maybe (BitreeZipperICP '[Lemma])
resolvePRO z = do cp0 <- snd . getRoot1 . current <$> parent z
                  atnode <- cp0^.cp_TP.tp_DP
                  case chooseATNode atnode of
                    SilentPRO -> Nothing
                    RExp x    -> Just x
                    


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



clauseStructure :: [VerbProperty (BitreeZipperICP '[Lemma])]
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


clauseForVerb :: [Range] -> VerbProperty a -> Maybe Range
clauseForVerb allrngs vp = case rngs of
                             [] -> Nothing
                             _  -> Just (minimumBy (compare `on` (\(b,e) -> e-b)) rngs)
  where i `isIn` (b,e) = b <= i && i <= e
        rngs = filter (\rng -> getAll (mconcat (map (\i -> All (i `isIn` rng)) (vp^..vp_words.traverse._2._1)))) allrngs




predicateArgWS :: CP -> ClauseTreeZipper -> PredArgWorkspace (Either (Range,STag) (Int,POSTag))
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
         -> VerbProperty (BitreeZipperICP '[Lemma])
         -> Maybe (PredArgWorkspace (Either (Range,STag) (Int,POSTag)))
findPAWS tr vp = do cp <- constructCP vp
                    predicateArgWS cp <$> findVerb (vp^.vp_index) tr



cutOutLevel0 :: ClauseTree -> ClauseTree
cutOutLevel0 x@(PL _             ) = x
cutOutLevel0 (PN (rng,(p,lvl)) xs) =
  if lvl == 0
  then case p of
         S_PP    _ -> PL (Left (rng,(p,lvl)))
         S_OTHER _ -> PL (Left (rng,(p,lvl)))
         _         -> PN (rng,(p,lvl)) (map cutOutLevel0 xs)
  else PN (rng,(p,lvl)) (map cutOutLevel0 xs)
