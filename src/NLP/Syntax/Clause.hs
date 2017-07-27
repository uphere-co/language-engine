{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Clause where

import           Control.Lens
import           Data.Bifoldable
import           Data.Either                     (partitionEithers)
import           Data.Function                   (on)
import           Data.IntMap                     (IntMap)
import           Data.List                       (minimumBy)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Format
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
-- import           SRL.Feature.Verb
-- import           SRL.Format
--import           SRL.Type.Clause
-- import           SRL.Type.Verb


currentlevel :: Bitree (Range,(STag,Int)) t -> Int
currentlevel (PN (_,(_,l)) _) = l
currentlevel (PL _ )          = 0


promoteToVP :: Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
            -> Either
                 (Int,(POSTag,Text))
                 (Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text))))
promoteToVP x@(PL (Right (i,(p,t))))  = if isVerb p || p == TO || p == MD
                                  then Left (i,(p,t))
                                  else Right x
promoteToVP x@(PL (Left _))       = Right x
promoteToVP (PN (_,(S_OTHER N.PRT,_)) (PL (Right (i,(p,t))):_)) = Left (i,(p,t))  -- for verb particle
promoteToVP x@(PN _ _)            = Right x


promoteNPPP :: Bitree (Range,(STag,Int)) t -> [Bitree (Range,(STag,Int)) t]
promoteNPPP x@(PN (_rng,(S_OTHER N.NP,_lvl)) [x1,x2]) =
  case (getRoot x1, getRoot x2) of
    (Left (_,(S_OTHER N.NP,_)), Left (_,(S_PP _,_))) ->  [x1,x2]
    _ -> [x]
promoteNPPP x = [x]



clauseStructure :: [VerbProperty]
                -> PennTreeIdxG N.CombinedTag (POSTag,Text)
                -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
clauseStructure _vps (PL (i,pt)) = PL (Right (i,pt))
clauseStructure vps  (PN (rng,tag) xs)
  = let ys = map (clauseStructure vps) xs
        (verbs,nonverbs0)= partitionEithers (map promoteToVP ys)
        nonverbs = concatMap promoteNPPP nonverbs0
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


findVerb :: Int
         -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
         -> Maybe (BitreeZipper (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text))))
findVerb i tr = getFirst (bifoldMap f f (mkBitreeZipper [] tr))
  where f x = First $ case getRoot (current x) of
                        Left (_,(S_VP lst,_))
                          -> if i `elem` (map (^._1) lst) then Just x else Nothing 
                        Right (Left (_,(S_VP lst,_)))
                          -> if i `elem` (map (^._1) lst) then Just x else Nothing
                        _ -> Nothing 



clauseRanges :: Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text))) -> [Range]
clauseRanges tr = bifoldMap f (const []) tr
  where f (rng,(S_CL _,_)) = [rng]
        f _                = []


clauseForVerb :: [Range] -> VerbProperty -> Maybe Range
clauseForVerb allrngs vp = case rngs of
                             [] -> Nothing
                             _  -> Just (minimumBy (compare `on` (\(b,e) -> e-b)) rngs)
  where i `isIn` (b,e) = b <= i && i <= e  
        rngs = filter (\rng -> getAll (mconcat (map (\i -> All (i `isIn` rng)) (vp^.vp_words)))) allrngs


verbArgs :: BitreeZipper (Range,(STag,Int))
                         (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
         -> VerbArgs (Either (Range,STag) (Int,POSTag))
verbArgs z = let (zfirst,str) = go (z,[]) z
             in VerbArgs { _va_string = str
                         , _va_arg0 = extractArg <$> prev zfirst
                         , _va_args = case child1 z of
                                        Nothing -> []
                                        Just z' ->
                                          map extractArg (z':iterateMaybe next z')
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
        go (z0,acc) y = case getRoot (current y) of
                          Left (_,(S_VP xs,_)) ->
                            let acc' = map snd xs ++ acc 
                            in case parent y of
                                 Nothing -> (y,acc')
                                 Just w -> go (y,acc') w
                          Right (Left (_,(S_VP xs,_))) ->
                            let acc' = map snd xs ++ acc 
                            in case parent y of
                                 Nothing -> (y,acc')
                                 Just w -> go (y,acc') w
                          _ -> (z0,acc)


getVerbArgs :: Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
            -> VerbProperty
            -> Maybe (VerbArgs (Either (Range,STag) (Int,POSTag)))
getVerbArgs tr vp = verbArgs <$> findVerb (vp^.vp_index) tr


cutOutLevel0 :: Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
             -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
cutOutLevel0 x@(PL _             ) = x
cutOutLevel0 (PN (rng,(p,lvl)) xs) =
  if lvl == 0
  then case p of
         S_PP    _ -> PL (Left (rng,(p,lvl)))
         S_OTHER _ -> PL (Left (rng,(p,lvl)))
         _         -> PN (rng,(p,lvl)) (map cutOutLevel0 xs)
  else PN (rng,(p,lvl)) (map cutOutLevel0 xs)


showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      tr' = bimap (\(_rng,x)->f x) g (cutOutLevel0 tr)
        where f (S_CL c,l)    = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,_l)   = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l)    = "ROOT" <> ":" <> T.pack (show l)
              g (Left x)      = T.pack (show x)
              g (Right x)     = T.pack (show x)

  T.IO.putStrLn (formatBitree id tr')

  let rngs = clauseRanges tr
  

  
  flip mapM_ vps $ \vp -> do
    putStrLn $ printf "%-50s | Clause %7s:  %s"
                 (formatVerbProperty vp)
                 (maybe "" show (clauseForVerb rngs vp))
                 (maybe "" formatVerbArgs (getVerbArgs tr vp))

