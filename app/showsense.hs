module Main where

import           Data.Discrimination                  (joining)
import           Data.Discrimination.Grouping
import           Data.Foldable                        (toList,traverse_)
import           Data.List                            (sort)
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import           System.Directory.Tree
import           System.FilePath
import           System.IO
--
import           OntoNotes.Parser.Sense

parseSenseFile :: FilePath -> IO (Either String [SenseInstance])
parseSenseFile fp = do
  txt <- T.IO.readFile fp
  let lst = T.lines txt
      wss = map T.words lst
  return (traverse parseSenseInst wss)
  --   mapM_ (print . parseSenseInst) wss


main :: IO ()
main = do
  putStrLn "OntoNotes: section Wall Street Journal"
  putStrLn "======================================"
  
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/00"

  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps

      joined = joining grouping headMatch takeBaseName takeBaseName sensefiles parsefiles
        where headMatch (x:_) (y:_) = Just (x,y)
              headMatch _     _     = Nothing
  -- traverse_ print joined
 
  flip traverse_ (catMaybes joined) $ \(fp_sense,fp_parse) -> do
    putStrLn (fp_sense)
    T.IO.putStrLn =<< T.IO.readFile fp_parse
    einsts <- parseSenseFile fp_sense
    case einsts of
      Left err -> error err
      Right insts -> traverse_ print insts
  
