{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Format where

import           Control.Lens
import           Data.Foldable                 (toList,traverse_)
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
import           NLP.Type.TagPos                        (TagPos,TokIdx)
import           Text.Format.Tree
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
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


formatTraceChain :: (a -> Text) -> TraceChain a -> Text
formatTraceChain f (TraceChain xs x) = T.concat (map ((<> " -> ") . fmt) xs) <> maybe "NOT_RESOLVED" f x
  where fmt NULL      = "*NUL*"
        fmt SilentPRO = "*PRO*"
        fmt Moved     = "*MOV*"
        fmt WHPRO     = "*WHP*"


showRange rng = T.pack (printf "%-7s" (show rng))


rangeText :: DetP as -> Text
rangeText x = x ^. headX . _2 . to show . to T.pack 

--   (T.pack . show . getRange . current) z
-- rangeText (Splitted x)   = (T.pack . show) (x ^. sdp_head)


formatDP x = case (x^.adjunct,x^.complement) of
               (Nothing,Nothing)    -> "DP"          <> rangeText x
               (Nothing,Just rng)   -> "DP-comp"     <> rangeText x
                                                     <> T.pack (show rng)
               (Just rng,Nothing)   -> "DP-adj"      <> rangeText x
                                                     <> T.pack (show rng)
               (Just rng,Just rng') -> "DP-comp-adj" <> rangeText x
                                                     <> T.pack (show rng')
                                                     <> T.pack (show rng)



formatDPorPP :: DPorPP (DetP as) -> Text
formatDPorPP (DP z)          = formatDP z
formatDPorPP (PrepP _mtxt z) = "PP-" <> formatDP z


formatPAWS :: PredArgWorkspace as (Either (Range,STag) (Int,POSTag))
           -> String
formatPAWS pa =
  printf "              subject       : %s\n\
         \              arg candidates: %s\n\
         \              complements   : %s"
         (formatTraceChain rangeText (pa^.pa_CP^.complement.specifier))
         ((intercalate " " . map (printf "%7s" . fmtArg)) (pa^.pa_candidate_args))
         (T.intercalate " | " (pa^..pa_CP.complement.complement.complement.traverse.to (formatTraceChain formatDPorPP)))
  where
    -- gettoken = map (tokenWord.snd) . toList . current
    fmtArg a = case a of
                 Right (_  ,p)           -> show p
                 Left  (_  ,(S_RT))      -> "ROOT"
                 Left  (rng,(S_SBAR _))  -> "SBAR" ++ show rng
                 Left  (rng,(S_CL c))    -> show c ++ show rng
                 Left  (_  ,(S_VP _))    -> "VP"
                 Left  (rng,(S_PP t b))  -> "(PP " ++ show t ++ if b then " ing" else "" ++ ")" ++ show rng
                 Left  (rng,(S_OTHER t)) -> show t ++ show rng


formatCP :: CP (Lemma ': as) -> String
formatCP cp = printf "Complementizer Phrase: %-4s  %s\n\
                     \Complementizer       : %-4s  %s\n\
                     \Tense Phrase         : %-4s  %s\n\
                     \Determiner Phrase    :       %s\n\
                     \Verb Phrase          : %-4s  %s\n\
                     \Verb Complements     :       %s\n"
                (maybe "null" show (getchunk =<< cp^.maximalProjection))
                (maybe "" (show . gettoken) (cp^.maximalProjection))
                (either show formatposchunk (fmap getposchunk (cp^.headX)))
                (either show (show . gettoken) (cp^.headX))
                (maybe "null" show (getchunk =<< cp^.complement.maximalProjection))
                (maybe "" (show . gettoken) (cp^.complement.maximalProjection))
                (formatTraceChain rangeText (cp^.complement.specifier))
                (maybe "null" show (getchunk (cp^.complement.complement.maximalProjection)))
                ((show . gettoken) (cp^.complement.complement.maximalProjection))
                ((T.intercalate " | " (cp^..complement.complement.complement.traverse.to (formatTraceChain formatDPorPP))))

  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current
        gettoken = map (tokenWord.snd) . toList . current
        getposchunk = bimap (chunkTag . snd) (posTag . snd) . getRoot . current
        formatposchunk (Left c) = show c
        formatposchunk (Right p) = "(" ++ show p ++ ")"




formatCPHierarchy :: Bitree (Range,CPDP as) (Range,CPDP as) -> Text
formatCPHierarchy tr = formatBitree fmt tr
  where
        fmt (rng,CPCase _) = "CP" <> showRange rng
        fmt (rng,DPCase x) = "DP" <> formatDP x

formatClauseStructure :: ClauseTree -> Text
formatClauseStructure clausetr =
  let tr' = bimap (\(_rng,x)->f x) g (cutOutLevel0 clausetr)
        where f (S_CL c,l)    = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p b,_l) = "PP:" <> T.pack (show p) <> if b then "-ing" else ""
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l)    = "ROOT" <> ":" <> T.pack (show l)
              g (Left x)      = T.pack (show x)
              g (Right x)     = T.pack (show x)
  in formatBitree id tr'


formatVPwithPAWS :: [TagPos TokIdx MarkType]
                 -> ClauseTree
                 -> [Bitree (Range,CPDP (Lemma ': as)) (Range,CPDP (Lemma ': as))]
                 -> VerbProperty (BitreeZipperICP (Lemma ': as))
                 -> Text
formatVPwithPAWS tagged clausetr cpstr vp =
  let mpaws = findPAWS tagged clausetr vp cpstr
      mrng = cpRange . (^.pa_CP) =<< mpaws
      fmt = either (const "") (tokenWord.snd) . getRoot . current
  in case (,) <$> mpaws <*> mrng of
       Nothing -> "fail in identifying PAWS"
       Just (paws,rng) -> T.pack (printf "%7s:%-50s\n%s\n"
                                   (show rng)
                                   (formatVerbProperty fmt vp)
                                   (maybe "" formatPAWS mpaws))
                          <> "\n"
                          <> T.pack (formatCP (paws^.pa_CP))
                          <> "\n"



showClauseStructure :: [TagPos TokIdx MarkType] -> IntMap Lemma -> PennTree -> IO ()
showClauseStructure tagged lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      cpstr = (map (bindingAnalysis tagged) . identifyCPHierarchy tagged) vps
      xs = map (formatVPwithPAWS tagged clausetr cpstr) vps
  mapM_ (T.IO.putStrLn . formatCPHierarchy) cpstr
  flip mapM_ xs (\vp -> putStrLn $ T.unpack vp)
