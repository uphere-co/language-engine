module Main where

import           Control.Lens
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
-- import           Data.IntMap.Merge.Lazy
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
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
--
import           SRL.Feature.Dependency
import           SRL.Init



main = do
  putStrLn "dependency"
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] mainInJVM


mainInJVM = do
  pp <- preparePP
  mainProcess pp

mainProcess pp = do
  let fp = "example2.txt"

  txt <- TIO.readFile fp
  ann <- annotate pp (Document txt (fromGregorian 2017 4 17))
  rdoc <- protobufDoc ann
  -- print rdoc
  void . runEitherT $ do
    doc <- hoistEither rdoc
    let sent = Seq.index (doc ^. D.sentence) 4
    dep <- hoistEither $ sentToDep sent
    liftIO $ print dep
    let tr = decodeToPennTree (fromJust (sent^.S.parseTree))
        itr = mkPennTreeIdx tr
        dtr = depLevelTree dep itr
        dtr' = depTree dep itr
        dtr'' = decorateLeaves (mergeMap (levelMap dep itr) (motherMap dep itr)) itr
    liftIO $ print dtr
    liftIO $ putStrLn "==============="
    liftIO $ print dtr''
    
    
