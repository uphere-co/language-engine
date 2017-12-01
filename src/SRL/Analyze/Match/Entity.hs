{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.Match.Entity where

import           Control.Applicative      ((<|>))
import           Control.Lens
import           Control.Monad            (guard,mzero)
import           Data.Foldable            (foldMap)
import           Data.Maybe               (fromMaybe,listToMaybe,maybeToList)
import           Data.Monoid              (First(..))
import           Data.Text                (Text)
import qualified Data.Text           as T
--
import           Data.BitreeZipper
import           Data.Range
import           NLP.Syntax.Clause        (currentCPDPPP)
import           NLP.Syntax.Type.XBar
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
--
import           SRL.Analyze.Type.Match   (DPInfo(..),EntityInfo(..))
--
-- import Debug.Trace



--
-- | this is very experimental now, only "the company" is checked.
--
definiteCorefResolution :: [X'Tree '[Lemma]]
                        -> TaggedLemma '[Lemma]
                        -> DetP '[Lemma]
                        -> Maybe (Range,Range)
definiteCorefResolution x'tr tagged dp = do
  let rng_dp = dp^.maximalProjection
  guard (dp^?headX.hd_class._Article == Just Definite)
  np <- dp^.complement
  let (b,e) = np^.maximalProjection
  guard (b == e)  -- a single word
  let ntxt = headText tagged np
  guard (ntxt == "company")  -- for the time being
  w <- getFirst (foldMap (First . extractZipperById rng_dp) x'tr)
  w' <- parent w
  cp' <- currentCPDPPP w' ^? _CPCase
  ((do w'' <- parent w'
       cp'' <- currentCPDPPP w'' ^? _CPCase
       dp'' <- cp''^?complement.specifier.trResolved._Just._Right
       nclass <- dp''^?complement._Just.headX.hn_class._Just
       if nclass == Org
         then return (rng_dp,dp''^.maximalProjection)
         else mzero)
   <|>
   (do cp'' <- cp'^?specifier._Just._SpecCP_Topic.trResolved._Just._CompVP_CP
       dp'' <- cp''^?complement.specifier.trResolved._Just._Right
       nclass <- dp''^?complement._Just.headX.hn_class._Just
       if nclass == Org
         then return (rng_dp,dp''^.maximalProjection)
         else mzero))



definiteGenitiveCorefResolution :: [X'Tree '[Lemma]]
                                -> TaggedLemma '[Lemma]
                                -> DetP '[Lemma]
                                -> Maybe (Range,Range)
definiteGenitiveCorefResolution x'tr tagged dp = do
  guard (dp^?headX.hd_class._GenitiveClitic == Just ())
  rng_specdp <- listToMaybe (dp^..specifier.traverse._SpDP_Gen)
  let stxt = T.intercalate " " (tokensByRange tagged rng_specdp)
  guard (stxt == "the company") -- for the time being
  w <- getFirst (foldMap (First . extractZipperById rng_specdp) x'tr)
  w' <- parent w
  cp' <- currentCPDPPP w' ^? _CPCase
  -- w'' <- parent w'                        -- this need to be revived.
  -- cp'' <- currentCPDPPP w'' ^? _CPCase
  dp' <- cp'^?complement.specifier.trResolved._Just._Right
  nclass <- dp'^?complement._Just.headX.hn_class._Just
  if nclass == Org
    then return (rng_specdp,dp'^.maximalProjection)
    else mzero


pronounResolution :: [X'Tree '[Lemma]]
                  -> DetP '[Lemma]
                  -> Maybe (Range,Range)
pronounResolution x'tr dp = do
    let rng_dp = dp^.maximalProjection
    rng_pro <- dp^.headX.hd_range
    (prnclass,isgenitive) <- dp^?headX.hd_class._Pronoun
    w <- getFirst (foldMap (First . extractZipperById rng_dp) x'tr)
    w' <- parent w
    cp' <- currentCPDPPP w' ^? _CPCase

    ((if isgenitive
        then do
          dp' <- cp'^?complement.specifier.trResolved._Just._Right
          nclass <- dp'^?complement._Just.headX.hn_class._Just
          match (rng_pro,prnclass) (nclass,dp')
        else mzero)
     <|>
     (do w'' <- parent w'
         cp'' <- currentCPDPPP w'' ^? _CPCase
         dp'' <- cp''^?complement.specifier.trResolved._Just._Right
         nclass <- dp''^?complement._Just.headX.hn_class._Just
         match (rng_pro,prnclass) (nclass,dp''))
     <|>
     (do cp'' <- cp'^?specifier._Just._SpecCP_Topic.trResolved._Just._CompVP_CP
         dp'' <- cp''^?complement.specifier.trResolved._Just._Right
         nclass <- dp''^?complement._Just.headX.hn_class._Just
         match (rng_pro,prnclass) (nclass,dp'')))
  where
    match (rng_pro,prnclass) (nclass,dp') =
      if | prnclass `elem` [P_He,P_She] && nclass == Person -> return (rng_pro,dp'^.maximalProjection)
         | prnclass `elem` [P_It]       && nclass == Org    -> return (rng_pro,dp'^.maximalProjection)
         | otherwise -> mzero

entityTextDP :: TaggedLemma t -> DetP t -> Text
entityTextDP tagged dp =
  case dp^.headX.hd_class of
    GenitiveClitic -> fromMaybe "" (fmap (headText tagged) (dp^.complement))
    _ -> T.intercalate " " (maybeToList (determinerText tagged (dp^.headX)) ++ maybeToList (fmap (headText tagged) (dp^.complement)))


entityFromDP :: [X'Tree '[Lemma]]
             -> TaggedLemma '[Lemma] -> DetP '[Lemma]
             -> (EntityInfo,DPInfo)
entityFromDP x'tr tagged dp =
  let rng = dp^.maximalProjection
      rnghead = fromMaybe rng (headRangeDP dp)
      headtxt = entityTextDP tagged dp

      mrngtxt' = do rng_sub <- listToMaybe (dp^..specifier.traverse._SpDP_Appos)
                    let txt_sub = T.intercalate " " (tokensByRange tagged rng_sub)
                    return (EI rng_sub rng_sub Nothing txt_sub)                 -- for the time being
      mcoref = pronounResolution x'tr dp <|> definiteCorefResolution x'tr tagged dp <|> definiteGenitiveCorefResolution x'tr tagged dp
      mcomp = do CompDP_PP pp <- dp^?complement._Just.complement._Just
                 dp' <- pp^?complement._CompPP_DP
                 let prep = pp^.headX.hp_prep
                 guard (prep == Prep_WORD "of")
                 let rng_comp = dp'^.maximalProjection
                     rng_head_comp = fromMaybe rng_comp (headRangeDP dp')
                     txt_comp = headTextDP tagged dp'
                 return (EI rng_comp rng_head_comp (Just "of") txt_comp)
      adjs  = do AdjunctDP_PP pp <- dp^.adjunct
                 dp' <- pp^..complement._CompPP_DP
                 let mprep = pp^?headX.hp_prep._Prep_WORD
                 let rng_adj = dp'^.maximalProjection
                     rng_head_adj = fromMaybe rng_adj (headRangeDP dp')
                     txt_adj = headTextDP tagged dp'
                 return (EI rng_adj rng_head_adj mprep txt_adj)
      mposs1 = do (_ptyp,True) <- dp^?headX.hd_class._Pronoun
                  rng_poss <- dp^.headX.hd_range
                  txt_poss <- determinerText tagged (dp^.headX)
                  return (EI rng_poss rng_poss Nothing txt_poss)
      mposs2 = do rng_poss <- listToMaybe (dp^..specifier.traverse._SpDP_Gen)
                  let txt_poss = T.intercalate " " (tokensByRange tagged rng_poss)
                  return (EI rng_poss rng_poss Nothing txt_poss)
      poss = maybeToList mposs1 ++ maybeToList mposs2
  in (EI rng rnghead Nothing headtxt, DI mrngtxt' mcoref mcomp poss adjs)
