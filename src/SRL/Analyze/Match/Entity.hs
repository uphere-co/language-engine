{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.Match.Entity where

import           Control.Lens
import           Control.Lens.Extras      (is)
import           Control.Monad            (guard)
import           Data.Foldable            (foldMap)
import           Data.List                (group,sort)
import           Data.Maybe               (fromMaybe,listToMaybe,maybeToList)
import           Data.Monoid              ((<>),First(..))
import           Data.Text                (Text)
import qualified Data.Text           as T
--
import           Data.BitreeZipper
import           Data.Range
import           NLP.Syntax.Clause        (currentCPDPPP)
import           NLP.Syntax.Type.XBar
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
import           WordNet.Query            (WordNetDB,getDerivations,lookupLemma)
import           WordNet.Type             (lex_word)
import           WordNet.Type.POS         (POS(..))
--
import           SRL.Analyze.Type         (DPInfo(..),EntityInfo(..))
--
import Debug.Trace


pronounResolution :: [X'Tree '[Lemma]]
                  -> DetP '[Lemma]
                  -> Maybe (Range,Range)
pronounResolution x'tr dp = do
  let rng_dp = dp^.maximalProjection
  rng_pro <- dp^.headX.hd_range
  prnclass <- dp^?headX.hd_class._Pronoun._1
  w <- getFirst (foldMap (First . extractZipperById rng_dp) x'tr)
  w' <- parent w
  guard (is _Just (currentCPDPPP w' ^? _CPCase))
  w'' <- parent w'
  cp <- currentCPDPPP w'' ^? _CPCase
  dp' <- cp^?complement.specifier.trResolved._Just._Right
  nclass <- dp'^?complement._Just.headX.hn_class._Just
  if | prnclass `elem` [P_He,P_She] && nclass == Person -> return (rng_pro,dp'^.maximalProjection)
     | prnclass `elem` [P_It]       && nclass == Org    -> return (rng_pro,dp'^.maximalProjection)
     | otherwise -> Nothing


entityTextDP :: TaggedLemma t -> DetP t -> Text
entityTextDP tagged dp =
  case dp^.headX.hd_class of
    GenitiveClitic -> fromMaybe "" (fmap (headText tagged) (dp^.complement))
    _ -> T.intercalate " " (maybeToList (determinerText tagged (dp^.headX)) ++ maybeToList (fmap (headText tagged) (dp^.complement)))
         <> let mpp = dp^?complement._Just.complement._Just._CompDP_PP
            in case mpp of
                 Nothing -> ""
                 Just pp -> " " <> (T.intercalate " " . tokensByRange tagged) (pp^.maximalProjection)



entityFromDP :: [X'Tree '[Lemma]]
             -> TaggedLemma '[Lemma] -> DetP '[Lemma]
             -> (EntityInfo,DPInfo)
entityFromDP x'tr tagged dp =
  let rng = dp^.maximalProjection
      rnghead = fromMaybe rng (headRangeDP dp)
      headtxt = entityTextDP tagged dp

      mrngtxt' = do rng_sub <- listToMaybe (dp^..specifier.traverse._SpDP_Appos)
                    let txt_sub = T.intercalate " " (tokensByRange tagged rng_sub)
                    return (EI rng_sub rng_sub txt_sub)                 -- for the time being
      mcoref = pronounResolution x'tr dp
      mcomp = do CompDP_PP pp <- dp^?complement._Just.complement._Just
                 dp' <- pp^?complement._CompPP_DP
                 let prep = pp^.headX.hp_prep
                 guard (prep == Prep_WORD "of")
                 let rng_comp = dp'^.maximalProjection
                     rng_head_comp = fromMaybe rng_comp (headRangeDP dp')
                     txt_comp = headTextDP tagged dp'
                 return (EI rng_comp rng_head_comp txt_comp)
      mposs1 = do (_ptyp,True) <- dp^?headX.hd_class._Pronoun
                  rng_poss <- dp^.headX.hd_range
                  txt_poss <- determinerText tagged (dp^.headX)
                  return (EI rng_poss rng_poss txt_poss)
      mposs2 = do rng_poss <- listToMaybe (dp^..specifier.traverse._SpDP_Gen)
                  let txt_poss = T.intercalate " " (tokensByRange tagged rng_poss)
                  return (EI rng_poss rng_poss txt_poss)
      poss = maybeToList mposs1 ++ maybeToList mposs2
  in (EI rng rnghead headtxt, DI mrngtxt' mcoref mcomp poss)
