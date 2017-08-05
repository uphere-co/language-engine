{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe                         (fromJust,fromMaybe,maybeToList)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Traversable
import qualified Data.Tree                  as Tree
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Verb
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util                     (merge)
import           Text.Format.Tree
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank


propbankCorpus ptreedir basedir article = do
  void . runEitherT $ do
    lst <- concat <$> loadMatchArticle ptreedir basedir article
    liftIO . flip mapM_ lst $ \(i,(((coretr,coredep,corelma),proptr),insts)) -> do
      putStrLn "\n\n\n-------------"
      putStrLn $ "sentence " ++ show i
      putStrLn "-------------"
      let tokens = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tokens)

      putStrLn "\n\n======"
      let minsts = matchInstances (coretr,proptr) insts
          lmap = IM.fromList (map (_2 %~ Lemma) corelma)
          verbprops = verbPropertyFromPennTree lmap coretr
          clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      
      showClauseStructure lmap coretr

      putStrLn "-----"
      putStrLn "propbank match test"
      putStrLn "-----"

      mapM_ (\minst -> putStrLn (formatMatchedVerb minst (matchVerbPropertyWithRelation verbprops clausetr minst))) minsts

      putStrLn "-----"
      putStrLn "dependency"
      putStrLn "-----"
      let tokens = map (^._2) . toList $ coretr
          tkmap = IM.fromList (zip [0..] tokens)
          deptree = fmap (\(i,r) -> let t = fromMaybe "" (IM.lookup (i-1) tkmap) in (i-1,t,r))
                      (dependencyLabeledTree coredep)
      T.IO.putStrLn (linePrint (T.pack.show) deptree)


main = do
  let --  article = "wsj_2445"
      ptreedir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
      -- framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"

  dtr <- build basedir
      
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
  flip mapM_ parsefiles $ \f -> do
    let article = takeBaseName f
    putStrLn "\n\n\n=============================================================================================="
    print article
    putStrLn "=============================================================================================="

    errorHandler stderr ("error happened in " ++ article) (propbankCorpus ptreedir basedir article)
