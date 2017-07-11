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
import           Data.Maybe                         (fromMaybe)
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
import           NLP.Type.PennTreebankII
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util                     (merge)
import           SRL.Feature.Clause
import           SRL.Feature.Verb
import           SRL.Type.Verb
import           Text.Format.Tree


prepare framedir basedir = do
  -- propdb <- constructFrameDB framedir
  -- let preddb = constructPredicateDB propdb
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
  return ((),props,trees) -- (preddb,props,trees)


readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readOrigPennTree pennfile
  = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode))
    =<< liftIO (T.IO.readFile pennfile)


readJSONList :: (FromJSON a) => FilePath -> EitherT String IO [a]
readJSONList file = EitherT $ eitherDecode <$> liftIO (BL.readFile file)


-- convertTop (PN _ xs) = PN "ROOT" xs


loadMatchArticle ptreedir framedir basedir article = do
  (preddb,props,trees) <- liftIO $ prepare framedir basedir
  let findf = find (\f -> takeBaseName f == article)
  flip traverse ((,) <$> findf props <*> findf trees) $ \(fprop,ftree) -> do
    insts <- readPropBank fprop 
    proptrs' <- readOrigPennTree ftree
    let proptrs = map convertTop proptrs'
        ptreefile = article <.> "corenlp_ptree"
        depfile = article <.> "corenlp_udep"
        plemmafile = article <.> "corenlp_lemma"
    coretrs :: [PennTree]  <- readJSONList  (ptreedir </> ptreefile)
    coredeps :: [Dependency] <- readJSONList (ptreedir </> depfile)    
    corelmas :: [[(Int,Text)]] <- readJSONList (ptreedir </> plemmafile)
    let cores = zip3 coretrs coredeps corelmas
        pairs = zip cores proptrs
    return (merge (^.inst_tree_id) pairs insts)

main = do
  let article = "wsj_2445"
      ptreedir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
      framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  void . runEitherT $ do
    lst' <- loadMatchArticle ptreedir framedir basedir article
    let lst = concat lst'
    liftIO . flip mapM_ lst $ \(i,(((coretr,coredep,corelma),proptr),insts)) -> do
      putStrLn "-------------"
      putStrLn $ "sentence " ++ show i
      let tokens = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tokens)
      putStrLn "--"
      putStrLn $ "propbank"
      let minsts = matchInstances (coretr,proptr) insts
      flip mapM_ minsts $ \minst-> do
        let inst = minst^.mi_instance
            args = minst^.mi_arguments
        print (findRelNode (minst^.mi_arguments),inst^.inst_lemma_roleset_id)
        mapM_ (print . (\a->(a^.ma_argument.arg_label.to pbLabelText,a^..ma_nodes.traverse.mn_node._1))) args
     
           
      putStrLn "--"
      putStrLn $ "verb property"
      let lmap = IM.fromList (map (_2 %~ Lemma) corelma)
      print $ map (\vp->(vp^.vp_index,vp^.vp_lemma.to unLemma)) (verbPropertyFromPennTree lmap coretr)
      showClauseStructure lmap coretr
      
      {-
      mapM_ printMatchedInst (matchInstances (coretr,proptr) insts)
       
      showClauseStructure lmap coretr
      let tokens = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tokens)
      let tkmap = IM.fromList (zip [0..] tokens)

          deptree = fmap (\(i,r) -> let t = fromMaybe "" (IM.lookup (i-1) tkmap) in (i-1,t,r))
                      (dependencyLabeledTree coredep)

      T.IO.putStrLn (linePrint (T.pack.show) deptree)
      -}
