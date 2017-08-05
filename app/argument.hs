{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Lens                hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Data.Foldable
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe                         (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           System.Directory.Tree
import           System.FilePath
import           System.IO
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Syntax.Clause
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Match
import           Text.Format.Tree
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Corpus.PropBank


propbankCorpus :: FilePath -> FilePath -> String -> IO ()
propbankCorpus ptreedir basedir article = do
  void . runEitherT $ do
    lst <- concat <$> loadMatchArticle ptreedir basedir article
    liftIO . flip mapM_ lst $ \(i,(((coretr,coredep,corelma),proptr),insts)) -> do
      putStrLn "\n\n\n-------------"
      putStrLn $ "sentence " ++ show i
      putStrLn "-------------"
      let tkns = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tkns)

      putStrLn "\n\n======"
      let minsts = matchInstances (coretr,proptr) insts
          lemmamap = IM.fromList (map (_2 %~ Lemma) corelma)
          verbprops = verbPropertyFromPennTree lemmamap coretr
          clausetr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      
      showClauseStructure lemmamap coretr

      putStrLn "-----"
      putStrLn "propbank match test"
      putStrLn "-----"

      mapM_ (\minst -> putStrLn (formatMatchedVerb minst (matchVerbPropertyWithRelation verbprops clausetr minst))) minsts

      putStrLn "-----"
      putStrLn "dependency"
      putStrLn "-----"
      let tkmap = IM.fromList (zip [0..] tkns)
          deptree = fmap (\(j,r) -> let t = fromMaybe "" (IM.lookup (j-1) tkmap) in (j-1,t,r))
                      (dependencyLabeledTree coredep)
      T.IO.putStrLn (linePrint (T.pack.show) deptree)


main :: IO ()
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
