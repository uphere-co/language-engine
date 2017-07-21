module Main where

import           Control.Lens          hiding (Level)

import           Control.Monad.Loops
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.Default
import qualified Data.Text                  as T
import           Language.Java         as J

import           System.Console.Haskeline
import           System.Environment

--
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified






queryProcess = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
    print input

main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                  )

    -- mapM_ (sentStructure pp . (^._3) ) ordered
    -- sentStructure pp txt
    queryProcess
