module Main where

import           Control.Lens
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Char8      as B
import           Data.Default
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




main = do
  putStrLn "dependency"
  let fp = "example2.txt"
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    txt <- TIO.readFile fp
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ False )
                   . ( depparse .~ True )
                   . ( constituency .~ True )
                   . ( ner .~ False )
    pp <- prepare pcfg
    ann <- annotate pp (Document txt (fromGregorian 2017 4 17))
    rdoc <- protobufDoc ann
    -- print rdoc
    void . runEitherT $ do
      doc <- hoistEither rdoc
      let sent = Seq.index (doc ^. D.sentence) 0
      dep <- hoistEither $ sentToDep sent
      liftIO $ print dep
      
