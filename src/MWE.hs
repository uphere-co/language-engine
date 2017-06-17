{-# LANGUAGE ScopedTypeVariables #-}

module MWE where

import           Control.Applicative               (many)
--
import           Control.Monad.Trans.Either        (EitherT(..))
import           Control.Monad.Trans.State
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Tree                         (Forest)
--
import           Generic.SearchTree                (addTreeItem)
-- import           SearchTree
import           ParserCustom

loadIdiom :: FilePath -> IO (Forest (Maybe String))
loadIdiom fp = do
  txt <- TIO.readFile fp 
  let (lst :: [[String]]) = map (read . T.unpack) $ drop 1 $ T.lines $ txt
      idiomsent = map head lst
      nentities = map words idiomsent
      forest = foldr addTreeItem [] nentities
  return forest


main :: IO ()
main = do
  putStrLn "search"
  forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
  let s = runState (runEitherT (many $ pTreeAdvG forest)) ["as","long","as","possible","take","care","of","away","from","I"]
  print s
  return ()
