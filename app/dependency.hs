module Main where

import           Control.Lens
import qualified Data.ByteString.Char8      as B
import           Data.Default
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar
import           Language.Java              as J
import           System.Environment
--
import           CoreNLP.Simple
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
    print rdoc
    -- void . runEitherT $ do
      
