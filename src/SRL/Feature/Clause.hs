{-# LANGUAGE OverloadedStrings #-}

module SRL.Feature.Clause where

import           Control.Lens
import           Data.Bifunctor
import           Data.Either                     (partitionEithers)
import           Data.Foldable
import           Data.IntMap                     (IntMap)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
--
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           SRL.Feature.Verb
import           SRL.Format
import           SRL.Type


data STag = S_RT
          | S_CL N.ClauseTag
          | S_VPBranch
          | S_VP [(POSTag,Text)]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show


currentlevel (PN (_,l) _) = l
currentlevel (PL _ )      = 0


verbCheck x@(PL (Right (p,t)))  = if isVerb p || p == TO
                                  then Left (p,t)
                                  else Right x
verbCheck x@(PL (Left _))       = Right x
-- verbCheck (PN (S_VP pts,_) _) = pts
verbCheck x@(PN _ _)            = Right x








clauseLevel :: [VerbProperty]
            -> PennTreeIdxG N.CombinedTag (POSTag,Text)
            -> Bitree (STag,Int) (Either N.PhraseTag (POSTag,Text))
clauseLevel vps (PN (rng,tag) xs)
  = let ys = map (clauseLevel vps) xs
        (verbs,nonverbs)= partitionEithers (map verbCheck ys)
        lvl = maximum (map currentlevel ys)
    in case tag of
         N.CL c -> PN (S_CL c,lvl+1) ys
         N.PH p -> case p of
                     N.VP -> PN (S_VP verbs,lvl) nonverbs
                     N.PP ->
                       case xs of
                         PL (_,(IN,t)):xs -> PN (S_PP t,lvl) (tail ys)
                         PL (_,(TO,t)):xs -> PN (S_PP t,lvl) (tail ys)
                         _                -> PL (Left p)
                     _    -> if lvl == 0
                             then PL (Left p) -- (T.pack (show p))
                             else PN (S_OTHER p,lvl  ) ys
         N.RT   -> PN (S_RT,lvl) ys
clauseLevel vps (PL (i,pt))
  = let verbs = filter (\is -> i `elem` is) . map (^.vp_words) $ vps 
    in PL (Right pt) -- (T.pack (show pt) <> T.pack (show verbs))



showClauseLevel :: IntMap Lemma -> PennTree -> IO ()
showClauseLevel lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseLevel vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      tr' = bimap f g tr
        where f (S_CL c,l) = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,l) = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l) = "ROOT" <> ":" <> T.pack (show l)
              g (Left x) = T.pack (show x)
              g (Right x) = T.pack (show x)
  T.IO.putStrLn (formatBitree id tr')

