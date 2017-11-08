{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.Match.Entity where

import           Control.Lens
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text as T
--
import           Data.Range
import           NLP.Syntax.Type.XBar


entityFromDP :: TaggedLemma t -> DetP t -> (Range,Text,Maybe (Range,Text))
entityFromDP tagged dp =
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
