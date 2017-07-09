{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text.IO               as T.IO
import           Data.Traversable
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Match                    (matchInstances, printMatchedInst)
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Prop
import           PropBank.Util                     (merge)


prepare framedir basedir = do
  propdb <- constructFrameDB framedir
  let preddb = constructPredicateDB propdb
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
  return (preddb,props,trees)


readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readPennTree pennfile = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode)) =<< liftIO (T.IO.readFile pennfile)

convertTop (PN _ xs) = PN "ROOT" xs


matchPropWithSerializedPennTree ptreedir framedir basedir article = do
  (preddb,props,trees) <- prepare framedir basedir
  let findf = find (\f -> takeBaseName f == article)
  flip traverse ((,) <$> findf props <*> findf trees) $ \(fprop,ftree) -> runEitherT $ do
    insts <- readPropBank fprop 
    proptrs' <- readPennTree ftree
    let proptrs = map convertTop proptrs'
        ptreefile = article <.> "corenlp_ptree"
    lbstr <- liftIO $ BL.readFile (ptreedir </> ptreefile)
    let mcoretrs = decode lbstr :: Maybe [PennTree]
    case mcoretrs of
      Nothing -> left "parse error corenlp tree"
      Just coretrs -> do
        let trs = zip coretrs proptrs
        return (merge (^.inst_tree_id) trs insts)


main = do
  let article = "wsj_2445"
      ptreedir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_20170702"
      framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  mlst <- matchPropWithSerializedPennTree ptreedir framedir basedir article
  case mlst of
    Nothing -> error "nothing"
    Just lst' -> do
      let lst = concat lst'
      flip mapM_ lst $ \(i,((coretr,proptr),insts)) -> do
        mapM_ printMatchedInst (matchInstances (coretr,proptr) insts)
