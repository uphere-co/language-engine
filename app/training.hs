{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels,(<.>))
import           Control.Exception
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Data.List                         (sort,zip4)
import           Data.Monoid                       ((<>))
import           Data.Maybe                        (mapMaybe)
import qualified Data.Sequence              as Seq
import           Language.Java              as J
import           System.Environment                (getEnv)
import           System.FilePath                   ((</>),(<.>))
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S

import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
--
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.DataSet.PropBank
import           SRL.Feature
import           SRL.PropBankMatch

  
process :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> (FilePath,FilePath)
        -> FilePath
        -> IO ()
process pp (dirpenn,dirprop) fp = do
  r <- try $ do    
    let pennfile = dirpenn </> fp <.> "mrg"
        propfile = dirprop </> fp <.> "prop"
    void . runEitherT $ do
      (trs,props) <- propbank (pennfile,propfile)
      rdocs <- liftIO $ do
        let docs = map mkDocFromPennTree trs
        anns <- mapM (annotate pp) docs
        rdocs <- mapM protobufDoc anns
        return rdocs
      ds <- mapM hoistEither rdocs
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
      deps <- hoistEither $ mapM sentToDep sents
      let cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          rs = map (\(i,((pt,tr,dep,sent),pr)) -> (i,SentInfo sent pt tr dep,pr))
             . merge (^.inst_tree_id) (zip4 pts trs deps sents)
             $ props
      liftIO $ mapM_ (showMatchedInstance <> showFeatures ) rs
  case r of
    Left (e :: SomeException) -> error $ "error in " ++ fp
    Right _ -> return ()

header fp = do
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn fp 
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"
  putStrLn "*****************************"


main :: IO ()
main = do
  let dirpenn = "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written"
      dirprop = "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written"
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ False )
                   . ( depparse .~ True )
                   . ( constituency .~ True )
                   . ( ner .~ False )
    pp <- prepare pcfg
    mapM_ (header <> process pp (dirpenn,dirprop)) (sort propbankFiles)
