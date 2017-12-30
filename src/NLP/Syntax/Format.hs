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
import           Text.Printf
--
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))
--
-- import           NLP.Syntax.Clause
import           NLP.Syntax.Format.Internal
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
-- import           NLP.Syntax.Util                        (rootTag)


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


formatVerbProperty :: (a -> Text) -> VerbProperty a -> String
formatVerbProperty f vp = printf "%3d %-15s : %-19s aux: %-7s neg: %-5s | %s"
                            (vp^.vp_index) (vp^.vp_lemma.to unLemma)
                            (formatTense  (vp^.vp_tense)  <> "." <>
                             formatAspect (vp^.vp_aspect) <> "." <>
                             formatVoice  (vp^.vp_voice))
                            (T.intercalate " " (vp^..vp_auxiliary.traverse._2._2.to unLemma))
                            (fromMaybe "" (vp^?vp_negation._Just._2._2.to unLemma))
                            (T.intercalate " " (vp^..vp_words.traverse.to (f.fst)))



-- formatAdjunctCP :: AdjunctCP 'PH0 -> Text
-- formatAdjunctCP (AdjunctCP_CP         cp) = "CP" <> showRange (cp^.maximalProjection)


adjunctVPText :: PreAnalysis t -> AdjunctVP 'PH0 -> Text
adjunctVPText tagged  (AdjunctVP_PP pp)         = T.intercalate " " (tokensByRange tagged (pp^.maximalProjection))


formatAdjunctVP :: AdjunctVP 'PH0 -> Text
formatAdjunctVP (AdjunctVP_PP pp)          = formatPP pp


formatCPDetail :: CP 'PH0 -> String
formatCPDetail cp =
  printf "Complementizer Phrase: %-6s\n\
         \Complementizer       : %-6s  %s\n\
         \Specifier            : %-6s  %s\n\
         \Adjunct              :         %s\n\
         \Tense Phrase         : %-6s\n\
         \Determiner Phrase    :         %s\n\
         \Verb Phrase          : %-6s\n\
         \Verb Complements     :       %s\n\
         \Verb Adjunts         :       %s\n"
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
        formatSpecCP :: Maybe SpecCP -> (String,String)
        formatSpecCP Nothing              = ("","")
        formatSpecCP (Just SpecCP_WHPHI)  = ("phi_WH","")
        formatSpecCP (Just (SpecCP_WH rng)) = ("WH",show rng)
        formatSpecCP (Just (SpecCP_Topic (SpecTopicP_CP rng))) = ("Topic","CP" ++ show rng)
        --
        (head1,head2) = fmtComplementizer (cp^.headX)
        (spec1,spec2) = formatSpecCP (cp^?specifier._Just.coidx_content)
