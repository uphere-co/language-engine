{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           AI.SVM.Simple
import           AI.SVM.Base
import           Control.Lens               hiding (levels,(<.>))
import           Control.Exception
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Data.Either                       (rights)
import           Data.List                         (sort,zip4)
import           Data.Monoid                       ((<>))
import           Data.Maybe                        (mapMaybe)
import qualified Data.Sequence              as Seq
import           Data.Vector.Storable (MVector (..),create)
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.JNI                as J
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
import           FastText.Binding
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.DataSet.PropBank
import           SRL.Feature
import           SRL.PropBankMatch
import           SRL.Train
import           SRL.Vectorize

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


preparePP :: IO (J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline"))
preparePP = do
  let pcfg = def & ( tokenizer .~ True )
                 . ( words2sentences .~ True )
                 . ( postagger .~ True )
                 . ( lemma .~ True )
                 . ( sutime .~ False )
                 . ( depparse .~ True )
                 . ( constituency .~ True )
                 . ( ner .~ False )
  prepare pcfg



run :: FastText -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> IO ()
run ft pp = do
  let dirpenn = "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written"
      dirprop = "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written"
  lsts <- flip mapM (take 10 propbankFiles) $ \(fp,omit) -> do
    r <- try $ do 
      header fp
      process ft pp (dirpenn,dirprop) (fp,omit)
    case r of
      Left (e :: SomeException) -> error $ "In " ++ fp ++ " exception : " ++ show e 
      Right (Right lst) -> return lst
      _ -> error "in run"
  let trainingData = concat lsts
  print (length trainingData)

  -- (msg,r) <- crossvalidate (C_SVC 1) (RBF 1) 2
  (msg,svm) <- trainSVM (C_SVC 1) (RBF 1) [] trainingData
  
  void $ test ft pp svm
  
  -- print (length (concat lsts))


initGHCi :: IO J.JVM
initGHCi = do
  clspath <- getEnv "CLASSPATH"
  j <- J.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] 
  return j

init2 :: IO FastText
init2 = initWVDB "/scratch/wavewave/wordvector/wiki.en.bin"


main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    t <- init2
    (preparePP >>= run t)
