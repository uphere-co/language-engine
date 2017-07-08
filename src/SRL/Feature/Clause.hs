{-# LANGUAGE OverloadedStrings #-}

module SRL.Feature.Clause where

import           Control.Lens
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Either                     (partitionEithers)
import           Data.Foldable
import           Data.IntMap                     (IntMap)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           SRL.Feature.Verb
import           SRL.Format
import           SRL.Type



data SBARType = SB_Word (POSTag,Text)
              | SB_WH   N.PhraseTag
              | SB_None
              deriving Show



data STag = S_RT
          | S_SBAR SBARType
          | S_CL N.ClauseTag
          | S_VP [(Int,(POSTag,Text))]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show


currentlevel (PN (_,(_,l)) _) = l
currentlevel (PL _ )          = 0


promoteToVP x@(PL (Right (i,(p,t))))  = if isVerb p || p == TO
                                  then Left (i,(p,t))
                                  else Right x
promoteToVP x@(PL (Left _))       = Right x
promoteToVP (PN (_,(S_OTHER N.PRT,_)) (PL (Right (i,(p,t))):_)) = Left (i,(p,t))  -- for verb particle
promoteToVP x@(PN _ _)            = Right x




clauseStructure :: [VerbProperty]
                -> PennTreeIdxG N.CombinedTag (POSTag,Text)
                -> Bitree (Range,(STag,Int)) (Either (Range,N.PhraseTag) (Int,(POSTag,Text)))
clauseStructure vps (PL (i,pt)) = PL (Right (i,pt))
clauseStructure vps (PN (rng,tag) xs)
  = let ys = map (clauseStructure vps) xs
        (verbs,nonverbs)= partitionEithers (map promoteToVP ys)
        lvl = maximum (map currentlevel ys) :: Int
    in case tag of
         N.CL c -> case c of
                     N.S    -> PN (rng,(S_CL c,lvl+1)) ys
                     N.SBAR ->
                       case xs of
                         PL (_,(IN,t))     : _ -> PN (rng,(S_SBAR (SB_Word (IN,t)),lvl)) (tail ys)
                         PN (_,(N.PH p)) _ : _ -> if N.isWHphrase p
                                                  then PN (rng,(S_SBAR (SB_WH p),lvl)) (tail ys)
                                                  else PN (rng,(S_SBAR SB_None,lvl)) ys
                         _other                -> PN (rng,(S_SBAR SB_None,lvl)) ys
                     _other -> PN (rng,(S_CL c,lvl)) ys
         N.PH p -> case p of
                     N.VP -> PN (rng,(S_VP verbs,lvl)) nonverbs
                     N.PP ->
                       case xs of
                         PL (_,(IN,t)):_ -> PN (rng,(S_PP t,lvl)) (tail ys)
                         PL (_,(TO,t)):_ -> PN (rng,(S_PP t,lvl)) (tail ys)
                         _               -> PL (Left (rng,p))
                     N.PRT ->
                       case xs of
                         PL (i,(p,t)):_  -> PN (rng,(S_OTHER N.PRT,lvl)) [PL (Right (i,(p,t)))]
                         _                -> PL (Left (rng,p))
                     _    -> if lvl == 0
                             then PL (Left (rng,p))
                             else PN (rng,(S_OTHER p,lvl)) ys
         N.RT   -> PN (rng,(S_RT,lvl)) ys 





findVerb :: Int
         -> Bitree (Range,(STag,Int)) (Either (Range,N.PhraseTag) (Int,(POSTag,Text)))
         -> Maybe (BitreeZipper (Range,(STag,Int)) (Either (Range,N.PhraseTag) (Int,(POSTag,Text))))
findVerb i tr = getFirst (bifoldMap f g (mkBitreeZipper [] tr))
  where f x = First $ case getRoot (current x) of
                        Left (_,(S_VP lst,_)) -> if i `elem` (map (^._1) lst)
                                                 then Just x
                                                 else Nothing
                        _                     -> Nothing 
        g _ = First Nothing



verbArgs :: BitreeZipper (Range,(STag,Int))
                         (Either (Range,N.PhraseTag) (Int,(POSTag,Text)))
         -> VerbArgs -- [(Range,(STag,Int))]
verbArgs z = VerbArgs (snd (go (z,[]) z))
  where go (z0,acc) z = case getRoot (current z) of
                          Left x@(_,(S_VP xs,_)) ->
                            let acc' = map snd xs ++ acc 
                            in case parent z of
                                 Nothing -> (z,acc')
                                 Just z' -> go (z,acc') z'
                          _ -> (z0,acc)


showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      tr' = bimap (\(rng,x)->f x) g tr
        where f (S_CL c,l) = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,l) = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l) = "ROOT" <> ":" <> T.pack (show l)
              g (Left x) = T.pack (show x)
              g (Right x) = T.pack (show x)

  T.IO.putStrLn (formatBitree id tr')
  let test = do z <- findVerb 4 tr
                return (verbArgs z)

  print test                
