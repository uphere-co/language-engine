{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Format
( module NLP.Syntax.Format
, module NLP.Syntax.Format.Internal
) where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text               as T
import           Formatting     ((%),(%.),sformat,stext,left,right,int)
--
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))
--
import           NLP.Syntax.Format.Internal
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar


formatTense :: Tense -> Text
formatTense Present = "Pres"
formatTense Past    = "Past"


formatAspect :: Aspect -> Text
formatAspect Simple             = "Smpl"
formatAspect Progressive        = "Prog"
formatAspect Perfect            = "Perf"
formatAspect PerfectProgressive = "PfPr"


formatVoice :: Voice -> Text
formatVoice Active  = "Actv"
formatVoice Passive = "Pass"

--   "%3d %-15s : %-19s aux: %-7s neg: %-5s | %s"
formatVerbProperty :: (a -> Text) -> VerbProperty a -> Text
formatVerbProperty f vp =
  sformat
    (   (right 3 ' ' %. int)
      %      " " % ls 15
      %    " : " % ls 19
      % " aux: " % ls 7
      % " neg: " % ls 5
      %    " | " % stext)
    (vp^.vp_index)
    (vp^.vp_lemma.to unLemma)
    (    formatTense  (vp^.vp_tense) <> "."
    <> formatAspect (vp^.vp_aspect)  <> "."
    <> formatVoice  (vp^.vp_voice))
    (T.intercalate " " (vp^..vp_auxiliary.traverse._2._2.to unLemma))
    (fromMaybe "" (vp^?vp_negation._Just._2._2.to unLemma))
    (T.intercalate " " (vp^..vp_words.traverse.to (f.fst)))


adjunctVPText :: PreAnalysis t -> AdjunctVP 'PH0 -> Text
adjunctVPText tagged  (AdjunctVP_PP pp)         = T.intercalate " " (tokensByRange tagged (pp^.maximalProjection))


formatAdjunctVP :: AdjunctVP 'PH0 -> Text
formatAdjunctVP (AdjunctVP_PP pp)          = formatPP pp


formatCPDetail :: CP 'PH0 -> String
formatCPDetail cp =
  stext
    (   "Complementizer Phrase: "%-6s
      % "\nComplementizer       : "%-6s"  " %s
      % "\nSpecifier            : "%-6s"  " %s
      % "\nAdjunct              :         "%s
      % "\nTense Phrase         : "%-6s"
      % "\nDeterminer Phrase    :         "%s
      % "\nVerb Phrase          : "%-6s
      % "\nVerb Complements     :       "%s
      % "\nVerb Adjunts         :       "%s % "\n"
    (show (cp^.maximalProjection))
    head1 head2
    spec1 spec2
    (T.intercalate " | " (cp^..adjunct.traverse.to (either (T.pack.show) (formatAdjunctCP SPH0))))
    (show (cp^.complement.maximalProjection))
    (formatCoindex (either (T.pack.show) (formatSpecTP SPH0)) (cp^.complement.specifier))
    (show (cp^.complement.complement.maximalProjection))
    ((T.intercalate " | " (cp^..complement.complement.complement.traverse.to (formatCoindex (either (T.pack.show) (formatCompVP SPH0))))))
    ((T.intercalate " | " (cp^..complement.complement.adjunct.traverse.to (either (T.pack.show) formatAdjunctVP))))

  where fmtComplementizer :: Complementizer -> (String,String)
        fmtComplementizer C_PHI = ("phi","")
        fmtComplementizer (C_WORD w) = ("",T.unpack (unLemma w))
        --
        fmtSpecCP :: Maybe SpecCP -> (String,String)
        fmtSpecCP Nothing              = ("","")
        fmtSpecCP (Just SpecCP_WHPHI)  = ("phi_WH","")
        fmtSpecCP (Just (SpecCP_WH rng)) = ("WH",show rng)
        fmtSpecCP (Just (SpecCP_Topic (SpecTopicP_CP rng))) = ("Topic","CP" ++ show rng)
        --
        (head1,head2) = fmtComplementizer (cp^.headX)
        (spec1,spec2) = fmtSpecCP (cp^?specifier._Just.coidx_content)
