{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.Match.Entity where

import           Control.Lens
import           Control.Monad  (mzero)
import           Data.Foldable  (foldMap)
import           Data.Monoid    ((<>),First(..))
import           Data.List      (find)
import           Data.Text      (Text)
import qualified Data.Text as T
--
import           Data.BitreeZipper
import           Data.Range
import           NLP.Syntax.Type.XBar
import           NLP.Type.PennTreebankII
--
import Debug.Trace


pronounResolution :: [X'Tree '[Lemma]] -> TaggedLemma t -> DetP t -> Maybe (Range,Text,Maybe (Range,Text))
pronounResolution x'tr tagged dp = do
  let rng_dp = dp^.maximalProjection
  w <- getFirst (foldMap (First . extractZipperById rng_dp) x'tr)
  trace "pronounResolution: here" $ 
    mzero


entityFromDP :: [X'Tree '[Lemma]] -> TaggedLemma t -> DetP t -> (Range,Text,Maybe (Range,Text))
entityFromDP x'tr tagged dp =
  case pronounResolution x'tr tagged dp of
    Just result -> result
    Nothing -> 
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
      in (rng,txt,mrngtxt')
