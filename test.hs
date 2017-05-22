module Main where

import Control.Lens
-- import qualified Data.HashSet as S
import System.Directory
import System.FilePath
--
import SRL.CoNLL.Parser


main = do
  let dir = "/scratch/wavewave/MASC/masc-conll/data/written"
  cnts <- getDirectoryContents dir
  let fs = filter (\x->takeExtensions x == ".conll") cnts
  flip mapM_ fs $  \f -> do
    let fp = dir </> f
    print fp
    sents <- parseFile fp
    let ds = do s <- sents
                l <- s^.sentence_lines
                return (l^.line_deprel)
    print (length ds)
  
