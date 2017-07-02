{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text         as A
import           Data.Foldable                        (toList,traverse_)
import           Data.List                            (sort)
import           Data.Text                            (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import           System.Directory.Tree
import           System.FilePath
import           System.IO
--
import           NLP.Type.PennTreebankII
import           NLP.Parser.PennTreebankII


getTerms :: PennTree -> [Text]
getTerms = map snd . filter (\(t,_) -> t /= "-NONE-") . toList 


main :: IO ()
main = do
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/23"
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps

  flip traverse_ (take 5 parsefiles) $ \f -> do
    putStrLn "\n\n\n=============================================================================================="
    print f
    putStrLn "=============================================================================================="
    etr <- fmap (A.parseOnly (A.many1 (A.skipSpace *> pnode))) (T.IO.readFile f)
    case etr of
      Left err -> print err
      Right trs -> do
        let tss = map getTerms trs
            ts = map (T.intercalate " ") tss
            ntxt = T.intercalate "\n\n" ts
        T.IO.putStrLn ntxt
