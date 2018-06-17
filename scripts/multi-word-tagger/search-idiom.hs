module Main where

import MWE.Idiom

main :: IO ()
main = do
  putStrLn "search"
  forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
  let s = runIdiomTagger forest
            ["as","long","as","possible","take","care","of","away","from","I"]
  print s
  return ()
