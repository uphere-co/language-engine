{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format where

import           Control.Lens
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
        

formatVerbProperty :: VerbProperty a -> String
formatVerbProperty vp = printf "%3d %-15s : %-35s  aux: %-7s neg: %-5s | %s"
                          (vp^.vp_index) (vp^.vp_lemma.to unLemma)
                          (show (vp^.vp_tense) ++ "." ++ show (vp^.vp_aspect) ++ "." ++ show (vp^.vp_voice))
                          (fromMaybe "" (vp^?vp_auxiliary._Just._2._2.to unLemma))
                          (fromMaybe "" (vp^?vp_negation._Just._2._2.to unLemma))
                          (show (vp^..vp_words.traverse._2._1))


formatVerbArgs :: VerbArgs (Either (Range,STag) (Int,POSTag)) -> String
formatVerbArgs va = printf "%10s %-20s %s"
                      (maybe "" fmtArg (va^.va_arg0))
                      (T.intercalate " " (map (^._2) (va^.va_string)))
                      ((intercalate " " . map (printf "%7s" . fmtArg)) (va^.va_args))
  where
    fmtArg a = case a of
                 Right (_  ,p)           -> show p
                 Left  (_  ,(S_RT))      -> "ROOT"
                 Left  (rng,(S_SBAR _))  -> "SBAR" ++ show rng
                 Left  (rng,(S_CL c))    -> show c ++ show rng
                 Left  (_  ,(S_VP _))    -> "VP"
                 Left  (rng,(S_PP t))    -> "(PP " ++ show t ++ ")" ++ show rng
                 Left  (rng,(S_OTHER t)) -> show t ++ show rng


formatCP :: ComplementPhrase -> String
formatCP cp = printf "Complement Phrase: %s\n\
                     \Tense Phrase     : %s\n\
                     \Verb Phrase      : %s"
                (maybe "null" show (getchunk =<< cp^.cp_governor))
                (maybe "null" show (getchunk =<< cp^.cp_TP.tp_governor))
                (maybe "null" show (getchunk (cp^.cp_TP.tp_VP)))
  where getchunk = either (Just . chunkTag . snd) (const Nothing) . getRoot . current



formatClauseStructure :: [VerbProperty a]
                      -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
                      -> [Text]
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
      rngs = clauseRanges clausetr
      xs = flip map vps $ \vp -> T.pack $ printf "%-50s | Clause %7s:  %s" (formatVerbProperty vp) (maybe "" show (clauseForVerb rngs vp)) (maybe "" formatVerbArgs (getVerbArgs clausetr vp))
  in [formatBitree id tr'] ++ xs


showClauseStructure :: IntMap Lemma -> PennTree -> IO ()
showClauseStructure lemmamap ptree  = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
  
      x:xs = formatClauseStructure vps clausetr
  T.IO.putStrLn x
  flip mapM_ xs (\vp -> putStrLn $ T.unpack vp)

