module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           System.FilePath
--
import           OntoNotes.Parser.Sense

main :: IO ()
main = do
  let dir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/00"
      filename = "wsj_0036.sense"

      fp = dir </> filename

  txt <- T.IO.readFile fp
  let lst = T.lines txt
      wss = map T.words lst
  mapM_ (print . parseSenseInst) wss
