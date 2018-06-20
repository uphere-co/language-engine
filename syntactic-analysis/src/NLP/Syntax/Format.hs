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
  where
    ls n = left n ' ' %. stext

adjunctVPText :: PreAnalysis t -> AdjunctVP 'PH0 -> Text
adjunctVPText tagged  (AdjunctVP_PP pp)         = T.intercalate " " (tokensByRange tagged (pp^.maximalProjection))


formatAdjunctVP :: AdjunctVP 'PH0 -> Text
formatAdjunctVP (AdjunctVP_PP pp)          = formatPP pp


formatCPDetail :: CP 'PH0 -> Text
formatCPDetail cp =
    sformat
      (   "Complementizer Phrase: "   % ls 6
        % "\nComplementizer       : " % ls 6 % "  " % stext
        % "\nSpecifier            : " % ls 6 % "  " % stext
        % "\nAdjunct              :         " % stext
        % "\nTense Phrase         : " % ls 6
        % "\nDeterminer Phrase    :         " % stext
        % "\nVerb Phrase          : " % ls 6
        % "\nVerb Complements     :       " % stext
        % "\nVerb Adjunts         :       " % stext % "\n")
      (T.pack (show (cp^.maximalProjection)))
      head1 head2
      spec1 spec2
      (T.intercalate " | " (cp^..adjunct.traverse.to (either (T.pack.show) (formatAdjunctCP SPH0))))
      (T.pack (show (cp^.complement.maximalProjection)))
      (formatCoindex (either (T.pack.show) (formatSpecTP SPH0)) (cp^.complement.specifier))
      (T.pack (show (cp^.complement.complement.maximalProjection)))
      ((T.intercalate " | " (cp^..complement.complement.complement.traverse.to (formatCoindex (either (T.pack.show) (formatCompVP SPH0))))))
      ((T.intercalate " | " (cp^..complement.complement.adjunct.traverse.to (either (T.pack.show) formatAdjunctVP))))

  where ls n = left n ' ' %. stext
        fmtComplementizer :: Complementizer -> (Text,Text)
        fmtComplementizer C_PHI = ("phi","")
        fmtComplementizer (C_WORD w) = ("",unLemma w)
        --
        fmtSpecCP :: Maybe SpecCP -> (Text,Text)
        fmtSpecCP Nothing              = ("","")
        fmtSpecCP (Just SpecCP_WHPHI)  = ("phi_WH","")
        fmtSpecCP (Just (SpecCP_WH rng)) = ("WH",T.pack (show rng))
        fmtSpecCP (Just (SpecCP_Topic (SpecTopicP_CP rng))) = ("Topic","CP" <> T.pack (show rng))
        --
        (head1,head2) = fmtComplementizer (cp^.headX)
        (spec1,spec2) = fmtSpecCP (cp^?specifier._Just.coidx_content)
