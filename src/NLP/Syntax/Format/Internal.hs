{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format.Internal where

import           Control.Lens                       ((^.),_2,to)
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
rangeText (Right x) = x ^. headX . _2 . to show . to T.pack
rangeText (Left x ) = (T.pack.show.getRange.current) x



formatDP :: DetP as -> Text
formatDP x = case (x^.adjunct,x^.complement) of
               (Nothing,Nothing)    -> "DP"          <> rangeText (Right x)
               (Nothing,Just rng)   -> "DP-comp"     <> rangeText (Right x)
                                                     <> T.pack (show rng)
               (Just rng,Nothing)   -> "DP-adj"      <> rangeText (Right x)
                                                     <> T.pack (show rng)
               (Just rng,Just rng') -> "DP-comp-adj" <> rangeText (Right x)
                                                     <> T.pack (show rng')
                                                     <> T.pack (show rng)


formatCompVP :: CompVP as -> Text
formatCompVP (CompVP_Unresolved z)  = "unresolved" <> T.pack (show (getRange (current z)))
formatCompVP (CompVP_CP z) = case z^.headX of
                               C_PHI -> "CP" <> rangeText (Left (z^.maximalProjection))
                               C_WORD z' -> "CP-" <> (T.intercalate "-" . map (tokenWord.snd) . toList . current) z'
                                                  <> rangeText (Left (z^.maximalProjection))
formatCompVP (CompVP_DP z)          = formatDP z
formatCompVP (CompVP_PP z) = "PP-" <> formatDP (z^.complement)



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

