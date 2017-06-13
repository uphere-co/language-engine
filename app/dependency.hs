module Main where

import           Control.Lens
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Foldable                     (toList)
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
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           SRL.Feature.Dependency
import           SRL.Feature.ParseTreePath
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
    let tr = decodeToPennTree (fromJust (sent^.S.parseTree))
        itr = mkPennTreeIdx tr
        dtr = depLevelTree dep itr
        dtr' = depTree dep itr
        ditr = depInfoTree dep itr
        -- dtr'' = decorateLeaves (mergeMap (levelMap dep itr) (motherMap dep itr)) itr
    liftIO $ print (motherMap dep itr)
    liftIO $ putStrLn "==============="
    liftIO $ print dep        
    liftIO $ putStrLn "==============="        
    liftIO $ print dtr'
    liftIO $ putStrLn "==============="
    liftIO $ print ditr 
    liftIO $ TIO.putStrLn $ prettyPrint 0 tr
    liftIO $ putStrLn "==============="
    let terms = map (^._2) . toList $ tr
    liftIO $ print (zip [0..] terms)

    liftIO $ putStrLn "==============="
    let dptp = parseTreePathFull (8,(9,10)) ditr
        f (PL (n,(md,_))) = ((n,n),md) -- fmap (^.dinfo_rel) md)
        f (PN (rng,(_,md)) _) = (rng,md) -- fmap (^.dinfo_rel) md)
    liftIO $ mapM_ print (parseTreePathBy f dptp)
