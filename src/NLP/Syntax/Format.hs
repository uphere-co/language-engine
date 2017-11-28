{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Format 
( module NLP.Syntax.Format
, module NLP.Syntax.Format.Internal
) where

import           Control.Lens
import           Data.Foldable                 (toList)
import           Data.IntMap                   (IntMap)
import           Data.List                     (intercalate)
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))
import           Text.Format.Tree
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format.Internal
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                        (rootTag)
import           NLP.Syntax.Verb


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))


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




showRange :: Range -> Text
showRange rng = T.pack (printf "%-7s" (show rng))




formatAdjunctCP :: AdjunctCP t -> Text
formatAdjunctCP (AdjunctCP_Unresolved z) = "unresolved" <> (showRange . getRange . current) z
formatAdjunctCP (AdjunctCP_CP         cp) = "CP" <> showRange (cpRange cp)


adjunctVPText tagged (AdjunctVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
adjunctVPText tagged (AdjunctVP_PP pp) = T.intercalate " " (tokensByRange tagged (pp^.maximalProjection))


formatAdjunctVP (AdjunctVP_Unresolved z) = showRange (getRange (current z))
formatAdjunctVP (AdjunctVP_PP pp) = formatPP pp


formatCP :: forall as. CP (Lemma ': as) -> String
formatCP cp = printf "Complementizer Phrase: %-6s  %s\n\
                     \Complementizer       : %-6s  %s\n\
                     \Specifier            : %-6s  %s\n\
                     \Adjunct              :         %s\n\
                     \Tense Phrase         : %-6s  %s\n\
                     \Determiner Phrase    :         %s\n\
                     \Verb Phrase          : %-6s  %s\n\
                     \Verb Complements     :       %s\n\
                     \Verb Adjunts         :       %s\n"
                (maybe "null" show (getchunk (cp^.maximalProjection)))
                (show (gettoken (cp^.maximalProjection)))
                head1 head2
                spec1 spec2
                (T.intercalate " | " (cp^..adjunct.traverse.to formatAdjunctCP))
                (maybe "null" show (getchunk (cp^.complement.maximalProjection)))
                (show (gettoken (cp^.complement.maximalProjection)))
                (formatTraceChain rangeText (cp^.complement.specifier))
                (maybe "null" show (getchunk (cp^.complement.complement.maximalProjection)))
                ((show . gettoken) (cp^.complement.complement.maximalProjection))
                ((T.intercalate " | " (cp^..complement.complement.complement.traverse.to (formatTraceChain formatCompVP ))))
                ((T.intercalate " | " (cp^..complement.complement.adjunct.traverse.to formatAdjunctVP)))

  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current
        gettoken = map (tokenWord.snd) . toList . current
        --
        -- formatAdjunctVP (AdjunctVP_Unresolved z) = T.intercalate " " (gettoken z)
        --
        formatposchunk (Left c) = show c
        formatposchunk (Right p) = "(" ++ show p ++ ")"
        --
        formatComplementizer :: Complementizer (Lemma ': as) -> (String,String)
        formatComplementizer C_PHI = ("phi","")
        formatComplementizer (C_WORD z) = (formatposchunk (rootTag (current z)), show (gettoken z))
        --
        formatSpecCP :: Maybe (SpecCP (Lemma ': as)) -> (String,String)
        formatSpecCP Nothing              = ("","")
        formatSpecCP (Just SpecCP_WHPHI)  = ("phi_WH","")
        formatSpecCP (Just (SpecCP_WH z)) = (formatposchunk (rootTag (current z)), show (gettoken z))
        formatSpecCP (Just (SpecCP_Topic _)) = ("Topic","Topic")
        --
        (head1,head2) = formatComplementizer (cp^.headX)
        (spec1,spec2) = formatSpecCP (cp^.specifier)



formatX'Tree :: X'Tree as -> Text
formatX'Tree tr = formatBitree fmt tr
  where
        fmt (rng,CPCase _) = "CP" <> showRange rng
        fmt (_  ,DPCase x) = formatDP x
        fmt (_  ,PPCase x) = formatPP x


