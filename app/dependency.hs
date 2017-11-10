{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Lens
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Foldable                     (toList)
import           Data.Maybe                        (fromJust)
import qualified Data.Sequence              as Seq
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar
import           Language.Java              as J
import           System.Environment
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           SRL.Old.Feature.Dependency
import           SRL.Old.Format
import           SRL.Old.Init


main :: IO ()
main = do
  putStrLn "dependency"
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] mainInJVM


mainInJVM :: IO ()
mainInJVM = do
  pp <- preparePP
  mainProcess pp


mainProcess :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> IO ()
mainProcess pp = do
  let fp = "example2.txt"
  txt <- TIO.readFile fp
  ann <- annotate pp (Document txt (fromGregorian 2017 4 17))
  rdoc <- protobufDoc ann
  void . runEitherT $ do
    doc <- hoistEither rdoc
    let sent = Seq.index (doc ^. D.sentence) 4
    dep <- hoistEither $ sentToDep sent
    let tr = decodeToPennTree (fromJust (sent^.S.parseTree))
        itr = mkPennTreeIdx tr

    liftIO $ print (motherMap dep)
    liftIO $ putStrLn "==============="
    liftIO $ print dep        
    liftIO $ TIO.putStrLn $ prettyPrint 0 tr
    liftIO $ putStrLn "==============="
    let terms = map (^._2) . toList $ tr
    liftIO $ print (zip ([0..] :: [Int]) terms)

    liftIO $ putStrLn "==============="

    let (start,target) = (8,(9,10))
        drelp = depRelPath dep itr (start,target)
    liftIO $ print (start,target)
    liftIO $ putStrLn (maybe "" formatDRP drelp)

    let (start2,target2) = (4,(17,18))
        drelp2 = depRelPath dep itr (start2,target2)
    liftIO $ print (start2,target2)
    liftIO $ putStrLn (maybe "" formatDRP drelp2)
