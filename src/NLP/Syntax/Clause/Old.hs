{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Clause.Old where

import           Control.Lens
import           Data.Bifoldable
import           Data.Either
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe,listToMaybe,mapMaybe)
import           Data.Monoid                  (First(..))
import           Data.Text                    (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Range
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Noun
import           NLP.Syntax.Preposition
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar


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
                                             let prep = fromMaybe "" (pp^?headX.hp_prep._Prep_WORD)
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
  let rng_cp = cp^.maximalProjection
  cp' <- (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById rng_cp)) x'tr)
  let zs_adj = mapMaybe (\rng_adj -> listToMaybe (extractZipperByRange rng_adj (tagged^.pennTree)))
                        (cp' ^.. complement.complement.adjunct.traverse._AdjunctVP_Unresolved)  -- can be dangerous
  predicateArgWS tagged cp' <$> findVerb (vp^.vp_index) tr <*> pure zs_adj


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
