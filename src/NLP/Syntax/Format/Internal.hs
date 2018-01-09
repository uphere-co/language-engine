{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),(^..),(^?),_Just,_Right,to)
import           Data.Bifunctor                     (bimap)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                     as T
import           Text.Printf                        (printf)
--
import           Data.Bitree
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
formatCoindex f x@(Coindex mi e) = formatCoindexOnly (either fmt f) x
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

{-
formatCP :: CP 'PH0 -> Text
formatCP cp =
  let rng = cp^.maximalProjection
  in "CP" <> showRange rng <>
     "[C:" <> formatComplementizer (cp^.headX) <>
     " spec: " <> maybe "" formatSpecCP (cp^.specifier) <>
     " (TP: spec:" <> formatCoindex (either (T.pack.show) formatSpecTP) (cp^.complement.specifier) <>
     " (VP: comp:" <>
     T.intercalate "," (cp^..complement.complement.complement.traverse.coidx_content._Right.to (either (T.pack.show) (formatCompVP SPH0))) <>
     "))]"
-}

formatX'Tree :: SPhase p -> X'Tree p -> Text
formatX'Tree s tr = formatBitree fmt tr
  where
        fmt (rng, CPCase x) = formatCP s x
        fmt (_  , DPCase x) = formatDP x
        fmt (_  , PPCase x) = formatPP x
        fmt (_  , APCase x) = formatAP x

{-
formatX'Tree SPH1 tr = formatBitree fmt tr
  where
        fmt (rng, CPCase x) = formatCP SPH1 x
        fmt (_  , DPCase x) = formatDP x
        fmt (_  , PPCase x) = formatPP x
        fmt (_  , APCase x) = formatAP x
-}



formatSpecCP :: SpecCP -> Text
formatSpecCP SpecCP_WHPHI     = "WHφ"
formatSpecCP (SpecCP_WH rng)  = "WH" <> T.pack (show rng)
formatSpecCP (SpecCP_Topic (SpecTopicP_CP rng)) = "Topic:CP" <> T.pack (show rng) -- (c^.maximalProjection)
--formatSpecCP (SpecCP_Topic (Left c)) = "Topic:Unresolved" <> T.pack (show c)
    -- formatCoindex (T.pack.show.either id (compVPToRange SPH0)) c
-- formatSpecCP (SpecCP_Topic c) = "Topic:CP" <> T.pack (show c) -- formatCoindex (T.pack.show.compVPToRange SPH1) c


{-
formatSpecCP1 :: SpecCP 'PH1 -> Text
formatSpecCP1 SpecCP_WHPHI = "WHφ"
formatSpecCP1 (SpecCP_WH rng) = "WH" <> T.pack (show rng)
formatSpecCP1 (SpecCP_Topic c) = "Topic" <> formatCoindex (T.pack.show.compVPToRange SPH1) c
-}

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

{-
formatCP SPH1 cp =
  let rng = cp^.maximalProjection
  in "CP" <> showRange rng <>
     "[C:" <> formatComplementizer (cp^.headX) <>
     " spec: " <> maybe "" (formatSpecCP SPH1) (cp^.specifier) <>
     " (TP: spec:" <> formatCoindex formatSpecTP (cp^.complement.specifier) <>
     " (VP: comp:" <> T.intercalate "," (cp^..complement.complement.complement.traverse.coidx_content._Right.to (formatCompVP SPH1)) <>
     "))]"
-}
