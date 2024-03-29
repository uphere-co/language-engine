{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),(^..),(^?),_Just,to)
import           Data.Bifunctor                     (bimap)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                     as T
import           Formatting                         ((%.),sformat,left,stext)
--
import           Data.Bitree
import           Data.Range                         (Range)
import           NLP.Type.PennTreebankII            (Lemma(..))
import           Text.Format.Tree
--
import           NLP.Syntax.Type.XBar


showRange :: Range -> Text
showRange rng = sformat (left 7 ' ' %. stext) (T.pack (show rng))


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))


formatSpecTP :: SPhase p -> SpecTP p -> Text
formatSpecTP SPH0 (SpecTP_DP x) = "DP" <> x^.maximalProjection.to (T.pack . show)
formatSpecTP SPH1 (SpecTP_DP x) = "DP" <> (T.pack . show) x


formatAdjunctCP :: SPhase p -> AdjunctCP p -> Text
formatAdjunctCP SPH0 (AdjunctCP_CP cp) = "CP" <> cp^.maximalProjection.to (T.pack . show)
formatAdjunctCP SPH1 (AdjunctCP_CP rng) = "CP" <> (T.pack . show) rng


formatComplementizer :: Complementizer -> Text
formatComplementizer C_PHI      = "φ"
formatComplementizer (C_WORD w) = unLemma w


formatCompDP :: CompDP -> Text
formatCompDP (CompDP_CP cp) = "CP" <> T.pack (show cp)
formatCompDP (CompDP_PP pp) = "PP" <> T.pack (show pp)



formatAdjunctDP :: AdjunctDP -> Text
formatAdjunctDP (AdjunctDP_AP rng) = "AP-" <> T.pack (show rng)
formatAdjunctDP (AdjunctDP_PP rng) = "PP-" <> T.pack (show rng)


formatPP :: PP p -> Text
formatPP pp = "PP" <> if pp^.headX.hp_pclass == PC_Time then "-time" else "" <> T.pack (show (pp^.maximalProjection))



formatDP :: DetP p -> Text
formatDP dp = "DP"        <> dp^.maximalProjection.to show.to T.pack <>
              " [D: "       <> maybe "" (T.pack.show) (dp^.headX.hd_range) <>
              "(NP:"      <> maybe "" formatNP (dp^? complement._Just) <> ")" <>
              " spec: "    <> T.intercalate " " (map (T.pack.show) (dp^.specifier)) <>
              " comp: "    <> maybe "" formatCompDP  (dp^?complement._Just.complement._Just) <>
              " adjunct: " <> (T.intercalate "," . map formatAdjunctDP) (dp^.adjunct) <>
              "]"


formatNP :: NounP p -> Text
formatNP np = np^.maximalProjection.to show.to T.pack <>
              np^.headX.coidx_content.hn_range.to show.to T.pack <>
              np^.headX.coidx_content.hn_class.to (maybe "" (T.pack.show)) <>
              np^.headX.coidx_i.to (maybe "" (\i -> "_" <> T.pack (show i))) 




formatAP :: AP p -> Text
formatAP ap = "AP" <> (ap^.maximalProjection.to show.to T.pack)


formatCompVP :: SPhase p -> CompVP p -> Text
formatCompVP SPH0 (CompVP_CP cp) = "CP" <> cp^.headX.to formatComplementizer <> cp^.maximalProjection.to show.to T.pack
formatCompVP SPH0 (CompVP_DP dp) = "DP" <> T.pack (show (dp^.maximalProjection))
formatCompVP SPH0 (CompVP_PP pp) = formatPP pp
formatCompVP SPH0 (CompVP_AP ap) = formatAP ap
formatCompVP SPH1 (CompVP_CP cp) = "CP" <> T.pack (show cp)
formatCompVP SPH1 (CompVP_DP dp) = "DP" <> T.pack (show dp)
formatCompVP SPH1 (CompVP_PP pp) = "PP" <> T.pack (show pp)
formatCompVP SPH1 (CompVP_AP ap) = "AP" <> T.pack (show ap)



formatCoindexOnly :: (a -> Text) -> Coindex a -> Text
formatCoindexOnly f (Coindex mi e) = f e <> maybe "" (\i -> "_" <> T.pack (show i)) mi

formatCoindex :: (a -> Text) -> Coindex (Either TraceType a) -> Text
formatCoindex f x@(Coindex _ _) = formatCoindexOnly (either fmt f) x
  where
    fmt NULL  = "NUL"
    fmt PRO   = "PRO"
    fmt Moved = "t"
    fmt WHPRO = "WHP"


formatX'Tree :: SPhase p -> X'Tree p -> Text
formatX'Tree s tr = formatBitree fmt tr
  where fmt (_, CPCase x) = formatCP s x
        fmt (_, DPCase x) = formatDP x
        fmt (_, PPCase x) = formatPP x
        fmt (_, APCase x) = formatAP x


formatSpecCP :: SpecCP -> Text
formatSpecCP SpecCP_WHPHI     = "WHφ"
formatSpecCP (SpecCP_WH rng)  = "WH" <> T.pack (show rng)
formatSpecCP (SpecCP_Topic (SpecTopicP_CP rng)) = "Topic:CP" <> T.pack (show rng)


formatCP :: SPhase p -> CP p -> Text
formatCP p cp =
  let rng = cp^.maximalProjection
  in "CP" <> showRange rng <>
     "[C:" <> formatComplementizer (cp^.headX) <>
     " spec: " <>
     maybe "" (formatCoindexOnly formatSpecCP) (cp^.specifier) <>
     " adjunct: " <>
     (case p of
        SPH0 -> T.intercalate "," (cp^..adjunct.traverse.to (either (T.pack.show) (formatAdjunctCP p)))
        SPH1 -> T.intercalate "," (cp^..adjunct.traverse.to (formatAdjunctCP p))) <>
     " (TP: spec:" <>
     (case p of
        SPH0 -> formatCoindex (either (T.pack.show) (formatSpecTP p)) (cp^.complement.specifier)
        SPH1 -> formatCoindex (formatSpecTP p) (cp^.complement.specifier)) <>
     " (VP: comp:" <>
     (case p of
        SPH0 -> T.intercalate "," (cp^..complement.complement.complement.traverse.to (formatCoindex (either (T.pack.show) (formatCompVP SPH0))))
        SPH1 -> T.intercalate "," (cp^..complement.complement.complement.traverse.to (formatCoindex (formatCompVP SPH1)))) <>
     "))]"

