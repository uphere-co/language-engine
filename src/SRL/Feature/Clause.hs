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



data SBARType = SB_Word (POSTag,Text)
              | SB_WH   N.PhraseTag
              | SB_None
              deriving Show



data STag = S_RT
          | S_SBAR SBARType
          | S_CL N.ClauseTag
          --   | S_VPBranch
          | S_VP [(POSTag,Text)]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show


currentlevel (PN (_,l) _) = l
currentlevel (PL _ )      = 0


promoteToVP x@(PL (Right (p,t)))  = if isVerb p || p == TO
                                  then Left (p,t)
                                  else Right x
promoteToVP x@(PL (Left _))       = Right x
promoteToVP (PN (S_OTHER N.PRT,_) (PL (Right (p,t)):_)) = Left (p,t)  -- for verb particle
promoteToVP x@(PN _ _)            = Right x




clauseStructure :: [VerbProperty]
                -> PennTreeIdxG N.CombinedTag (POSTag,Text)
                -> Bitree (STag,Int) (Either N.PhraseTag (POSTag,Text))
clauseStructure vps (PL (i,pt)) = PL (Right pt)
clauseStructure vps (PN (rng,tag) xs)
  = let ys = map (clauseStructure vps) xs
        (verbs,nonverbs)= partitionEithers (map promoteToVP ys)
        lvl = maximum (map currentlevel ys)
    in case tag of
         N.CL c -> case c of
                     N.S    -> PN (S_CL c,lvl+1) ys
                     N.SBAR ->
                       case xs of
                         PL (_,(IN,t))     : _ -> PN (S_SBAR (SB_Word (IN,t)),lvl) (tail ys)
                         PN (_,(N.PH p)) _ : _ -> if N.isWHphrase p
                                                  then PN (S_SBAR (SB_WH p),lvl) (tail ys)
                                                  else PN (S_SBAR SB_None,lvl) ys
                         _                     -> PN (S_SBAR SB_None,lvl) ys
                     _   -> PN (S_CL c,lvl  ) ys
         N.PH p -> case p of
                     N.VP -> PN (S_VP verbs,lvl) nonverbs
                     N.PP ->
                       case xs of
                         PL (_,(IN,t)):_ -> PN (S_PP t,lvl) (tail ys)
                         PL (_,(TO,t)):_ -> PN (S_PP t,lvl) (tail ys)
                         _               -> PL (Left p)
                     N.PRT ->
                       case xs of
                         PL (_,(p,t)):_  -> PN (S_OTHER N.PRT,lvl) [PL (Right (p,t))]
                         _                -> PL (Left p)
                     _    -> if lvl == 0
                             then PL (Left p)
                             else PN (S_OTHER p,lvl  ) ys
         N.RT   -> PN (S_RT,lvl) ys

-- let verbs = filter (\is -> i `elem` is) . map (^.vp_words) $ vps


showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      tr' = bimap f g tr
        where f (S_CL c,l) = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,l) = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l) = "ROOT" <> ":" <> T.pack (show l)
              g (Left x) = T.pack (show x)
              g (Right x) = T.pack (show x)
  T.IO.putStrLn (formatBitree id tr')

