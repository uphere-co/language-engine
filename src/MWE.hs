module MWE where

import           Control.Applicative          (many)
--
import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.State
-- import           Control.Monad.State.Lazy
--
import           Generic.SearchTree
-- import           SearchTree
import           ParserCustom

main :: IO ()
main = do
  putStrLn "search"
  forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
  let s = runState (runEitherT (many $ pTreeAdvG forest)) ["as","long","as","possible","take","care","of","away","from","I"]
  print s
  return ()
