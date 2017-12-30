{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Format.Old where

import           Control.Lens
import           Data.IntMap                  (IntMap)
import           Data.List                    (intercalate)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Range
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Clause.Old
import           NLP.Syntax.Format
import           NLP.Syntax.Verb
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar


formatPAWS :: PredArgWorkspace (Either (Range,STag) (Int,POSTag)) -> String
formatPAWS pa =
  printf "              subject       : %s\n\
         \              arg candidates: %s\n\
         \              complements   : %s"
         (formatCoindex (either (T.pack.show) (formatSpecTP SPH0)) (pa^.pa_CP^.complement.specifier))
         ((intercalate " " . map (printf "%7s" . fmtArg)) (pa^.pa_candidate_args))
         (T.intercalate " | " (pa^..pa_CP.complement.complement.complement.traverse.to (formatCoindex (either (T.pack.show) (formatCompVP SPH0)))))
  where
    fmtArg a = case a of
                 Right (_  ,p)           -> show p
                 Left  (_  ,(S_RT))      -> "ROOT"
                 Left  (rng,(S_SBAR _))  -> "SBAR" ++ show rng
                 Left  (rng,(S_CL c))    -> show c ++ show rng
                 Left  (_  ,(S_VP _))    -> "VP"
                 Left  (rng,(S_PP t c b))  -> "(PP " ++ show t ++ " " ++ show c ++ if b then " ing" else "" ++ ")" ++ show rng
                 Left  (rng,(S_OTHER t)) -> show t ++ show rng

formatVPwithPAWS :: PreAnalysis (Lemma ': as)
                 -> ClauseTree
                 -> [X'Tree 'PH0]
                 -> VerbProperty (BitreeZipperICP (Lemma ': as))
                 -> Text
formatVPwithPAWS tagged clausetr cpstr vp =
  let mpaws = findPAWS tagged clausetr vp cpstr
      mrng = mpaws^?_Just.pa_CP.maximalProjection
      fmt = either (const "") (tokenWord.snd) . getRoot . current
  in case (,) <$> mpaws <*> mrng of
       Nothing -> "fail in identifying PAWS"
       Just (paws,rng) -> T.pack (printf "%7s:%-50s\n%s\n"
                                   (show rng)
                                   (formatVerbProperty fmt vp)
                                   (maybe "" formatPAWS mpaws))
                          <> "\n"
                          <> formatCP SPH0 (paws^.pa_CP)
                          <> "\n"


formatClauseStructure :: ClauseTree -> Text
formatClauseStructure clausetr =
  let tr' = bimap (\(_rng,x)->f x) g (cutOutLevel0 clausetr)
        where f (S_CL c,l)    = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p c b,_l) = "PP:" <> T.pack (show p) <> " " <> T.pack (show c) <> if b then "-ing" else ""
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l)    = "ROOT" <> ":" <> T.pack (show l)
              g (Left x)      = T.pack (show x)
              g (Right x)     = T.pack (show x)
  in formatBitree id tr'



showClauseStructure :: PreAnalysis '[Lemma] -> IntMap Lemma -> PennTree -> IO ()
showClauseStructure tagged lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      clausetr = clauseStructure tagged vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      x'trs = identifyCPHierarchy tagged vps
        -- (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps
      xs = map (formatVPwithPAWS tagged clausetr x'trs) vps
  mapM_ (T.IO.putStrLn . formatX'Tree SPH0) x'trs
  flip mapM_ xs (\vp -> putStrLn $ T.unpack vp)
