{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),(^?),_Just,to)
import           Data.Bifunctor                     (bimap)
import           Data.Foldable                      (toList)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                     as T
import           Text.Printf                        (printf)
--
import           Data.Bitree
import           Data.BitreeZipper                  (current)
import           Data.ListZipper                    (ListZipper(LZ))
import           Data.Range                         (Range)
import           NLP.Type.PennTreebankII            (getRange,tokenWord)
import           Text.Format.Tree
--
import           NLP.Syntax.Type.XBar


showRange :: Range -> Text
showRange rng = T.pack (printf "%-7s" (show rng))


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))

formatSpecTP :: SpecTP as -> Text
formatSpecTP (SpecTP_DP x) = x^.maximalProjection.to (T.pack . show)
formatSpecTP (SpecTP_Unresolved x) = (T.pack . show) x


formatCompDP :: CompDP t -> Text
formatCompDP (CompDP_Unresolved rng) = T.pack (show rng)
formatCompDP (CompDP_CP cp) = case cp^.headX of
                                C_PHI -> "CP" <> cp^.maximalProjection.to show.to T.pack
                                C_WORD z' -> "CP-" <> (T.intercalate "-" . map (tokenWord.snd) . toList . current) z'
                                                   <> cp^.maximalProjection.to show.to T.pack
formatCompDP (CompDP_PP pp) = formatPP pp



formatAdjunctDP :: AdjunctDP t -> Text
formatAdjunctDP (AdjunctDP_Unresolved rng) = T.pack (show rng)
formatAdjunctDP (AdjunctDP_PP pp) = "(" <> formatPP pp <> ")"


formatPP :: PP t -> Text
formatPP pp = "PP" <> T.pack (show (pp^.maximalProjection)) <>
              "-" <>
              case pp^.complement of
                CompPP_DP dp    -> formatDP dp
                CompPP_Gerund z -> "ing" <> T.pack (show (getRange (current z)))


formatDP :: DetP t -> Text
formatDP dp = "(DP"        <> dp^.maximalProjection.to show.to T.pack <>
              "[D: "       <> maybe "" (T.pack.show) (dp^.headX.hd_range) <>
              " NP: "      <> maybe "" (T.pack.show) (dp^?complement._Just.headX.hn_range) <>
              " spec: "    <> T.intercalate " " (map (T.pack.show) (dp^.specifier)) <>
              " comp: "    <> maybe "" formatCompDP  (dp^?complement._Just.complement._Just) <>
              " adjunct: " <> (T.intercalate "," . map formatAdjunctDP) (dp^.adjunct) <>
              "])"


formatCompVP :: CompVP as -> Text
formatCompVP (CompVP_Unresolved z)  = "unresolved" <> T.pack (show (getRange (current z)))
formatCompVP (CompVP_CP cp) = case cp^.headX of
                                C_PHI -> "CP" <> cp^.maximalProjection.to show.to T.pack
                                C_WORD z' -> "CP-" <> (T.intercalate "-" . map (tokenWord.snd) . toList . current) z'
                                                   <> cp^.maximalProjection.to show.to T.pack
formatCompVP (CompVP_DP dp) = formatDP dp
formatCompVP (CompVP_PP pp) = formatPP pp



formatTraceChain :: (a -> Text) -> TraceChain a -> Text
formatTraceChain f (TraceChain xs0 x) =
    case xs0 of
      Left (LZ ps c ns) -> fmtLst (reverse ps) <> "[" <> fmt c <> "] -> " <> fmtLst ns <> fmtResolved x
      Right xs          -> fmtLst xs <> "[" <> fmtResolved x <> "]"
  where
    fmt NULL      = "*NUL*"
    fmt SilentPRO = "*PRO*"
    fmt Moved     = "*MOV*"
    fmt WHPRO     = "*WHP*"
    --
    fmtLst = T.concat . map ((<> " -> ") . fmt)
    --
    fmtResolved = maybe "NOT_RESOLVED" f


formatX'Tree :: X'Tree as -> Text
formatX'Tree tr = formatBitree fmt tr
  where
        fmt (rng,CPCase _) = "CP" <> showRange rng
        fmt (_  ,DPCase x) = formatDP x
        fmt (_  ,PPCase x) = formatPP x

