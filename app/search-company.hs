module Main where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Loops
import qualified Data.Text.IO             as TIO
import           Data.Tree
import           System.Console.Haskeline
--
import           Generic.SearchTree  hiding (makeF7745Forest)
--
import           MWE.Company


main :: IO ()
main = do
  putStrLn "search"
  txt <- TIO.readFile "/data/groups/uphere/data/Wiki/F7745.all_entities"
  let forest = makeF7745Forest txt
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $ do
    print $ searchFunc forest input

