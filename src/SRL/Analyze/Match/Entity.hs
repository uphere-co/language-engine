{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SRL.Analyze.Match.Entity where

import           Control.Lens
import           Control.Lens.Extras      (is)
import           Control.Monad            (guard,mzero)
import           Data.Foldable            (foldMap)
import           Data.Monoid              ((<>),First(..))
import           Data.List                (find)
import           Data.Text                (Text)
import qualified Data.Text           as T
--
import           Data.BitreeZipper
import           Data.Range
import           NLP.Syntax.Clause        (currentCPDPPP)
import           NLP.Syntax.Type.XBar
import           NLP.Type.PennTreebankII
--
import Debug.Trace


pronounResolution :: [X'Tree '[Lemma]]
                  -> TaggedLemma '[Lemma]
                  -> DetP '[Lemma]
                  -> Maybe Range
pronounResolution x'tr tagged dp = do
  let rng_dp = dp^.maximalProjection
  prnclass <- dp^?headX.hd_class._Pronoun
  w <- getFirst (foldMap (First . extractZipperById rng_dp) x'tr)
  w' <- parent w
  guard (is _Just (currentCPDPPP w' ^? _CPCase))
  w'' <- parent w'
  cp <- currentCPDPPP w'' ^? _CPCase
  dp' <- cp^?complement.specifier.trResolved._Just._Right
  return (dp'^.headX.hd_range) --  (entityFromDP x'tr tagged dp')

data DPInfo = DI { _adi_appos :: Maybe (Range,Text)
                 , _adi_coref :: Maybe Range
                 }


makeLenses ''DPInfo


entityFromDP :: [X'Tree '[Lemma]] -> TaggedLemma '[Lemma] -> DetP '[Lemma] -> (Range,Text,DPInfo)
entityFromDP x'tr tagged dp =
  let rng = dp^.headX.hd_range
      headtxt = headText tagged dp
      txt = case dp^.complement of
              Just (CompDP_PP pp) ->
                let prep = pp^.headX.hp_prep
                    rng_pp = pp^.maximalProjection
                in if prep == Prep_WORD "of"
                   then headtxt <> " " <> T.intercalate " " (tokensByRange tagged rng_pp)
                   else headtxt
              _ -> headtxt
      mrngtxt' = do rng_sub <- dp^.specifier  -- for the time being, specifier is used as attribute appositive
                    let txt_sub = T.intercalate " " (tokensByRange tagged rng_sub)
                    return (rng_sub,txt_sub)
      mcoref = pronounResolution x'tr tagged dp
  in (rng,txt,DI mrngtxt' mcoref)
