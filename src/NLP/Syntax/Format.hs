{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format where

import           Control.Lens
import           Data.Foldable                 (toList)
import           Data.IntMap                   (IntMap)
import           Data.List                     (intercalate)
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Data.Tree               as Tr
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           Lexicon.Type                           (chooseATNode)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           Text.Format.Tree
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Verb


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))
  where toTree (PN x xs) = Tr.Node x (map toTree xs)
        toTree (PL x)    = Tr.Node x []

        
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
                            (fromMaybe "" (vp^?vp_auxiliary._Just._2._2.to unLemma))
                            (fromMaybe "" (vp^?vp_negation._Just._2._2.to unLemma))
                            (T.intercalate " " (vp^..vp_words.traverse.to (f.fst)))



formatPAWS :: PredArgWorkspace (Either (Range,STag) (Int,POSTag)) -> String
formatPAWS pa = printf "              subject       : %s\n\
                       \              arg candidates: %s\n\
                       \              complements   : %s"
                  (maybe "" (T.intercalate " " . gettoken) (pa^.pa_CP.cp_TP.tp_DP.to (fmap chooseATNode)))
                  ((intercalate " " . map (printf "%7s" . fmtArg)) (pa^.pa_candidate_args))
                  ((intercalate " | " . map (T.unpack . T.intercalate " ". gettoken)) (pa^.pa_CP.cp_TP.tp_VP.vp_complements))
                  
  where
    gettoken = map (tokenWord.snd) . toList . current
    
    fmtArg a = case a of
                 Right (_  ,p)           -> show p
                 Left  (_  ,(S_RT))      -> "ROOT"
                 Left  (rng,(S_SBAR _))  -> "SBAR" ++ show rng
                 Left  (rng,(S_CL c))    -> show c ++ show rng
                 Left  (_  ,(S_VP _))    -> "VP"
                 Left  (rng,(S_PP t))    -> "(PP " ++ show t ++ ")" ++ show rng
                 Left  (rng,(S_OTHER t)) -> show t ++ show rng


formatCP :: CP -> String
formatCP cp = printf "Complementizer Phrase: %-4s  %s\n\
                     \Complementizer       : %-4s  %s\n\
                     \Tense Phrase         : %-4s  %s\n\
                     \Determiner Phrase    : %-4s  %s\n\
                     \Verb Phrase          : %-4s  %s\n\
                     \Verb Complements     : %s\n"
                (maybe "null" show (getchunk =<< cp^.cp_maximal_projection))
                (maybe "" (show . gettoken) (cp^.cp_maximal_projection))
                (maybe "null" formatposchunk (fmap getposchunk (cp^.cp_complementizer)))
                (maybe "" (show . gettoken) (cp^.cp_complementizer))
                (maybe "null" show (getchunk =<< cp^.cp_TP.tp_maximal_projection))
                (maybe "" (show . gettoken) (cp^.cp_TP.tp_maximal_projection))
                (maybe "null" show (getchunk =<< cp^.cp_TP.tp_DP.to (fmap chooseATNode)))
                (maybe "" (show . gettoken) (cp^.cp_TP.tp_DP.to (fmap chooseATNode)))
                (maybe "null" show (getchunk (cp^.cp_TP.tp_VP.vp_maximal_projection)))
                ((show . gettoken) (cp^.cp_TP.tp_VP.vp_maximal_projection))
                ((intercalate " | " . map (show . gettoken)) (cp^.cp_TP.tp_VP.vp_complements))

  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current
        gettoken = map (tokenWord.snd) . toList . current
        getposchunk = bimap (chunkTag . snd) (posTag . snd) . getRoot . current
        formatposchunk (Left c) = show c
        formatposchunk (Right p) = "(" ++ show p ++ ")"


formatClauseStructure :: [VerbProperty (BitreeZipperICP '[Lemma])]
                      -> ClauseTree
                      -> Text
formatClauseStructure vps clausetr =
  let tr' = bimap (\(_rng,x)->f x) g (cutOutLevel0 clausetr)
        where f (S_CL c,l)    = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,_l)   = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l)    = "ROOT" <> ":" <> T.pack (show l)
              g (Left x)      = T.pack (show x)
              g (Right x)     = T.pack (show x)

  in formatBitree id tr'

formatVPwithPAWS :: ClauseTree
                 -> VerbProperty (BitreeZipperICP '[Lemma])
                 -> Text
formatVPwithPAWS clausetr vp =
  let rngs = clauseRanges clausetr
      fmt = either (const "") (tokenWord.snd) . getRoot . current
  in T.pack $ printf "%7s:%-50s\n%s\n"
                (maybe "" show (clauseForVerb rngs vp))
                (formatVerbProperty fmt vp)
                (maybe "" formatPAWS (findPAWS clausetr vp))


showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
  
      x = formatClauseStructure vps clausetr
      xs = map (formatVPwithPAWS clausetr) vps
  T.IO.putStrLn x
  flip mapM_ xs (\vp -> putStrLn $ T.unpack vp)

