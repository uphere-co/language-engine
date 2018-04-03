{-# LANGUAGE DataKinds #-}

module SRL.Old.Init where

import           Control.Lens
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Foreign.JNI                as J
import           Language.Java              as J
import           System.Environment
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type


initGHCi :: IO J.JVM
initGHCi = do
  clspath <- getEnv "CLASSPATH"
  J.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] 


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
