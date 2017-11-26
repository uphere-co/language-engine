{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.UKB where

import           Control.Applicative                          ((<|>))
import           Control.Error.Safe
import           Control.Lens                                 ((^.),(^..),(.~),(%~),to,toListOf)
import           Control.Monad                                (guard)
import           Data.Foldable                                (toList,traverse_)
import           Data.Maybe                                   (mapMaybe)
import           Data.List                                    (intercalate)
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import           Data.Text.Read                               (decimal)
import           Text.Printf                                  (printf)
--
import           CoreNLP.Simple.Convert                       (mkLemmaMap)
import           HUKB.PPR                                     (ppr)
import           HUKB.Type                                    (UKBResultWord(..),UKBResult(..),Context(..),ContextWord(..)
                                                              ,ukbresult_words,ukbrw_id,ukbrw_syn,ukbrw_word)
import           WordNet.Type.Lexicographer                   (LexicographerFile(..))
import           WordNet.Query                                (WordNetDB,lookupSynset)
import           WordNet.Type                                 (LexItem(..),Pointer(..),SynsetOffset(..))
import           WordNet.Type.POS                             (POS(..))
import           NLP.Type.CoreNLP                             (Sentence,sentenceLemma)
--

import qualified Data.Text.IO as TIO
import Data.Attribute (ahead)
import NLP.Syntax.Util (mkBitreeICP)
import           NLP.Type.PennTreebankII                      (Lemma(..),tokenWord,posTag,getAnnot,isNoun,isVerb,TernaryLogic(..))
import           NLP.Printer.PennTreebankII                   (prettyPrint)
-- import NLP.Syntax.

posTagToPOS p = (guard (isVerb p)        >> return POS_V) <|>
                (guard (isNoun p == Yes) >> return POS_N) <|>
                Nothing

letterToPOS 'n' = Just POS_N
letterToPOS 'v' = Just POS_V
letterToPOS 'a' = Just POS_A
letterToPOS 'r' = Just POS_R
letterToPOS _   = Nothing


ukbLookupSynset :: WordNetDB -> UKBResultWord Text -> UKBResultWord (Maybe LexicographerFile)
ukbLookupSynset db u = let s = do (s0,pos) <- parsesyn (u^.ukbrw_syn)
                                  (fn,_,_,_) <- lookupSynset db pos (SynsetOffset s0)
                                  guard (fn >= 0 && fn <= 44)
                                  return (toEnum fn)
                       in (ukbrw_syn .~ s) u
  where
    parsesyn txt = do (n,txt') <- rightMay (decimal txt)
                      let txt'' = (T.drop 1 txt')
                      if (not (T.null txt''))
                        then (n,)<$>letterToPOS (T.head txt'')
                        else Nothing
                     
    

formatUKBResult r = intercalate "\n" $ map formatUKBResultWord (r^.ukbresult_words)
  where
    formatUKBResultWord u = printf "%-3d: %-20s: %s" (u^.ukbrw_id) (u^.ukbrw_word) (maybe "" (T.pack . show) (u^.ukbrw_syn)) :: String

runUKB wndb (sents,parsetrees) = do
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
  flip mapM_ (zip lmass parsetrees) $ \(lmas,mpt) ->
    flip traverse_ mpt $ \pt -> do
      let lmap = (mkLemmaMap . map unLemma) lmas
          lemmapt = mkBitreeICP lmap pt
          mkContextWord (i,x) = CtxtWord (unLemma (ahead (getAnnot x))) <$> posTagToPOS (posTag x) <*> pure i <*> pure 1
          ctxt = Context "0" (mapMaybe mkContextWord (toList lemmapt))
      -- print ctxt
      r <- ppr ctxt
      let r' = (ukbresult_words %~ map (ukbLookupSynset wndb)) r
      putStrLn ("\n\nTHIS RESULT: \n" ++ formatUKBResult r')


