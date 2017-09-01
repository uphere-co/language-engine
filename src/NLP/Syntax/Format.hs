{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Format where

import           Control.Lens
import           Control.Monad                 (void)
-- import           Control.Monad.IO.Class        (liftIO)
-- import           Control.Monad.Trans.State     (State,StateT(..),execState,get,put)
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
import           Lexicon.Type                           (chooseATNode)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.SyntaxProperty                (Tense(..),Voice(..),Aspect(..))
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
                            (fromMaybe "" (vp^?vp_auxiliary._Just._2._2.to unLemma))
                            (fromMaybe "" (vp^?vp_negation._Just._2._2.to unLemma))
                            (T.intercalate " " (vp^..vp_words.traverse.to (f.fst)))


formatDPTokens :: [Either NTrace (Zipper as)] -> Text
formatDPTokens xs = T.intercalate " -> " (map fmt xs)
  where fmt (Left NULL)      = "*NUL*"
        fmt (Left SilentPRO) = "*PRO*"
        fmt (Left Moved)     = "*MOV*"
        fmt (Left WHPRO)     = "*WHP*"
        fmt (Right z) = T.pack (show (getRange (current z)))

{- 
formatDPType :: ATNode (DP (Zipper as)) -> Maybe ChunkTag
formatDPType x = case chooseATNode x of
                   SilentPRO -> Just NP
                   RExp z -> getchunk z
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current
-}

formatPAWS :: Maybe [Bitree (Range,CP as) (Range,CP as)]
           -> PredArgWorkspace as (Either (Range,STag) (Int,POSTag))
           -> String
formatPAWS mcpstr pa =
  printf "              subject       : %s\n\
         \              arg candidates: %s\n\
         \              complements   : %s"
         (formatDPTokens (pa^.pa_CP^.complement.specifier)) -- ,resolveDP mcpstr (pa^.pa_CP)))
         ((intercalate " " . map (printf "%7s" . fmtArg)) (pa^.pa_candidate_args))
         ((intercalate " | " . map (T.unpack . T.intercalate " ". gettoken)) (pa^.pa_CP.complement.complement.complement))
                  
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


formatCP :: Maybe [Bitree (Range,CP (Lemma ': as)) (Range,CP (Lemma ': as))]
         -> CP (Lemma ': as)
         -> String
formatCP mcpstr cp
            = printf "Complementizer Phrase: %-4s  %s\n\
                     \Complementizer       : %-4s  %s\n\
                     \Tense Phrase         : %-4s  %s\n\
                     \Determiner Phrase    :       %s\n\
                     \Verb Phrase          : %-4s  %s\n\
                     \Verb Complements     : %s\n"
                (maybe "null" show (getchunk =<< cp^.maximalProjection))
                (maybe "" (show . gettoken) (cp^.maximalProjection))
                (maybe "null" formatposchunk (fmap getposchunk (cp^.headX)))
                (maybe "" (show . gettoken) (cp^.headX))
                (maybe "null" show (getchunk =<< cp^.complement.maximalProjection))
                (maybe "" (show . gettoken) (cp^.complement.maximalProjection))
                -- (maybe "null" show (cp^.complement.specifier.to (fmap formatDPType)))
                (formatDPTokens (cp^.complement.specifier))  -- ,resolveDP mcpstr cp))
                (maybe "null" show (getchunk (cp^.complement.complement.maximalProjection)))
                ((show . gettoken) (cp^.complement.complement.maximalProjection))
                ((intercalate " | " . map (show . gettoken)) (cp^.complement.complement.complement))

  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current
        gettoken = map (tokenWord.snd) . toList . current
        getposchunk = bimap (chunkTag . snd) (posTag . snd) . getRoot . current
        formatposchunk (Left c) = show c
        formatposchunk (Right p) = "(" ++ show p ++ ")"


formatCPHierarchy :: Bitree (Range,CP as) (Range,CP as) -> Text
formatCPHierarchy tr = formatBitree (\(rng,_cp) -> T.pack (printf "%-7s" (show rng))) tr


formatClauseStructure :: ClauseTree -> Text
formatClauseStructure clausetr =
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
                 -> Maybe [Bitree (Range,CP (Lemma ': as)) (Range,CP (Lemma ': as))]
                 -> VerbProperty (BitreeZipperICP (Lemma ': as))
                 -> Text
formatVPwithPAWS clausetr mcpstr vp =
  let -- rngs = clauseRanges clausetr
      mpaws = findPAWS clausetr vp mcpstr
      mrng = cpRange . (^.pa_CP) =<< mpaws
      fmt = either (const "") (tokenWord.snd) . getRoot . current
  in case (,) <$> mpaws <*> mrng of
       Nothing -> "fail in identifying PAWS"
       Just (paws,rng) -> T.pack (printf "%7s:%-50s\n%s\n"
                                   (show rng) -- (maybe "" show (clauseForVerb rngs vp))
                                   (formatVerbProperty fmt vp)
                                   (maybe "" (formatPAWS mcpstr) mpaws))
                          <> "\n"
                          <> T.pack (formatCP mcpstr (paws^.pa_CP))
                          <> "\n"
                          

showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      mcpstr = (fmap (map bindingAnalysis) . identifyCPHierarchy) vps
      xs = map (formatVPwithPAWS clausetr mcpstr) vps
  traverse_ (mapM_ (T.IO.putStrLn . formatCPHierarchy)) mcpstr
  flip mapM_ xs (\vp -> putStrLn $ T.unpack vp)

  -- traverse_ (mapM_ bindingAnalysis) mcpstr



