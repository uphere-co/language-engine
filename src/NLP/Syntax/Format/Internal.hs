{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),to)
import           Data.Foldable                      (toList)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                     as T
--
import           Data.BitreeZipper                  (current)
import           Data.ListZipper                    (ListZipper(LZ))
import           NLP.Type.PennTreebankII            (getRange,tokenWord)
--
import           NLP.Syntax.Type.XBar


rangeText :: Either (Zipper as) (DetP as) -> Text
rangeText (Right x) = x ^. headX . to show . to T.pack
rangeText (Left x ) = (T.pack.show.getRange.current) x


formatCompDP :: CompDP t -> Text
formatCompDP (CompDP_Unresolved rng) = T.pack (show rng)
formatCompDP (CompDP_CP cp) = case cp^.headX of
                                C_PHI -> "CP" <> rangeText (Left (cp^.maximalProjection))
                                C_WORD z' -> "CP-" <> (T.intercalate "-" . map (tokenWord.snd) . toList . current) z'
                                                   <> rangeText (Left (cp^.maximalProjection))
formatCompDP (CompDP_PP pp) = formatPP pp



formatAdjunctDP :: AdjunctDP t -> Text
formatAdjunctDP (AdjunctDP_Unresolved rng) = T.pack (show rng)
formatAdjunctDP (AdjunctDP_PP pp) = formatPP pp


formatPP :: PP t -> Text
formatPP pp = "PP-" <> case pp^.complement of
                         CompPP_DP dp    -> formatDP dp
                         CompPP_Gerund z -> "ing" <> T.pack (show (getRange (current z)))


formatDP :: DetP t -> Text
formatDP dp = "DP"         <> rangeText (Right dp) <>
              " comp: "    <> maybe "" formatCompDP  (dp^.complement) <>
              " adjunct: " <> (T.intercalate " " . map formatAdjunctDP) (dp^.adjunct)



formatCompVP :: CompVP as -> Text
formatCompVP (CompVP_Unresolved z)  = "unresolved" <> T.pack (show (getRange (current z)))
formatCompVP (CompVP_CP cp) = case cp^.headX of
                                C_PHI -> "CP" <> rangeText (Left (cp^.maximalProjection))
                                C_WORD z' -> "CP-" <> (T.intercalate "-" . map (tokenWord.snd) . toList . current) z'
                                                   <> rangeText (Left (cp^.maximalProjection))
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

