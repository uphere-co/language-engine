{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),(^..),(^?),_Just,_Right,to)
import           Data.Bifunctor                     (bimap)
-- import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                     as T
import           Text.Printf                        (printf)
--
import           Data.Bitree
-- import           Data.BitreeZipper                  (current)
import           Data.ListZipper                    (ListZipper(LZ))
import           Data.Range                         (Range)
import           NLP.Type.PennTreebankII            (Lemma(..))
import           Text.Format.Tree
--
import           NLP.Syntax.Type.XBar


showRange :: Range -> Text
showRange rng = T.pack (printf "%-7s" (show rng))


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))

formatSpecTP :: SpecTP -> Text
formatSpecTP (SpecTP_DP x) = x^.maximalProjection.to (T.pack . show)
formatSpecTP (SpecTP_Unresolved x) = (T.pack . show) x


formatComplementizer :: Complementizer -> Text
formatComplementizer C_PHI      = ""
formatComplementizer (C_WORD w) = "-" <> unLemma w


formatCompDP :: CompDP -> Text
-- formatCompDP (CompDP_Unresolved rng) = T.pack (show rng)
formatCompDP (CompDP_CP cp) = "CP" <> T.pack (show cp)

  -- cp^.headX.to formatComplementizer <> cp^.maximalProjection.to show.to T.pack
formatCompDP (CompDP_PP pp) = "PP" <> T.pack (show pp) -- formatPP pp



formatAdjunctDP :: AdjunctDP -> Text
formatAdjunctDP (AdjunctDP_AP rng) = "AP-" <> T.pack (show rng)
formatAdjunctDP (AdjunctDP_PP rng) = "PP-" <> T.pack (show rng)


formatPP :: PP -> Text
formatPP pp = "PP" <> if pp^.headX.hp_pclass == PC_Time then "-time" else "" <> T.pack (show (pp^.maximalProjection))
{-                <>
              "-" <>
              case pp^.complement of
                CompPP_DP dp      -> formatDP dp
                CompPP_Gerund rng -> "ing" <> T.pack (show rng)
-}



formatDP :: DetP -> Text
formatDP dp = "(DP"        <> dp^.maximalProjection.to show.to T.pack <>
              "[D: "       <> maybe "" (T.pack.show) (dp^.headX.hd_range) <>
              "(NP:"      <> maybe "" formatNP (dp^? complement._Just) <> ")" <>
              " spec: "    <> T.intercalate " " (map (T.pack.show) (dp^.specifier)) <>
              " comp: "    <> maybe "" formatCompDP  (dp^?complement._Just.complement._Just) <>
              " adjunct: " <> (T.intercalate "," . map formatAdjunctDP) (dp^.adjunct) <>
              "])"


formatNP :: NounP -> Text
formatNP np = (np^.maximalProjection.to show.to T.pack) <>
              (np^.headX.hn_range.to show.to T.pack) <>
              fromMaybe "" (np^?complement._Just.to compDPToRange.to show.to T.pack)


formatAP :: AP -> Text
formatAP ap = "AP" <> (ap^.maximalProjection.to show.to T.pack)



formatCompVP :: CompVP -> Text
formatCompVP (CompVP_Unresolved r)  = "unresolved" <> T.pack (show r)
formatCompVP (CompVP_CP cp) = "CP" <> cp^.headX.to formatComplementizer <> cp^.maximalProjection.to show.to T.pack
formatCompVP (CompVP_DP dp) = "DP" <> T.pack (show (dp^.maximalProjection)) --  formatDP dp
formatCompVP (CompVP_PP pp) = formatPP pp
formatCompVP (CompVP_AP ap) = formatAP ap

formatCoindex :: (a -> Text) -> Coindex a -> Text
formatCoindex f (Coindex mi e) = either fmt f e <> maybe "" (\i -> "_" <> T.pack (show i)) mi
  where
    fmt NULL  = "NUL"
    fmt PRO   = "PRO"
    fmt Moved = "t"
    fmt WHPRO = "WHP"

{- 
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
-}

formatX'Tree :: X'Tree -> Text
formatX'Tree tr = formatBitree fmt tr
  where
        fmt (rng, CPCase x) = "CP" <> showRange rng <> ": VP-comps: " <> T.intercalate "," (x^..complement.complement.complement.traverse.coidx_content._Right.to formatCompVP)
        fmt (_  , DPCase x) = formatDP x
        fmt (_  , PPCase x) = formatPP x
        fmt (_  , APCase x) = formatAP x
