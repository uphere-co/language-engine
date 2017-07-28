{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Format where

import           Control.Lens
import           Data.List                     (intercalate)
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text               as T
import           Data.Tree               as Tr
import           Text.Printf
--
import           Data.Bitree
import           NLP.Type.PennTreebankII
import           Text.Format.Tree
--
import           NLP.Syntax.Type


formatBitree :: (a -> Text) ->  Bitree a a -> Text
formatBitree fmt tr = linePrint fmt (toTree (bimap id id tr))
  where toTree (PN x xs) = Tr.Node x (map toTree xs)
        toTree (PL x)    = Tr.Node x []
        

formatVerbProperty :: VerbProperty -> String
formatVerbProperty vp = printf "%3d %-15s : %-35s  aux: %-7s neg: %-5s | %s"
                          (vp^.vp_index) (vp^.vp_lemma.to unLemma)
                          (show (vp^.vp_tense) ++ "." ++ show (vp^.vp_aspect) ++ "." ++ show (vp^.vp_voice))
                          (fromMaybe "" (vp^?vp_auxiliary._Just._2.to unLemma))
                          (fromMaybe "" (vp^?vp_negation._Just._2.to unLemma))
                          (show (vp^.vp_words))


formatVerbArgs :: VerbArgs (Either (Range,STag) (Int,POSTag)) -> String
formatVerbArgs va = printf "%10s %-20s %s"
                      (maybe "" formatArg (va^.va_arg0))
                      (T.intercalate " " (map (^._2) (va^.va_string)))
                      ((intercalate " " . map (printf "%7s" . formatArg)) (va^.va_args))
  where
    formatArg a = case a of
                    Right (_  ,p)           -> show p
                    Left  (_  ,(S_RT))      -> "ROOT"
                    Left  (rng,(S_SBAR _))  -> "SBAR" ++ show rng
                    Left  (rng,(S_CL c))    -> show c ++ show rng
                    Left  (_  ,(S_VP _))    -> "VP"
                    Left  (rng,(S_PP t))    -> "(PP " ++ show t ++ ")" ++ show rng
                    Left  (rng,(S_OTHER t)) -> show t ++ show rng

