module Main where

import           Data.Aeson
import qualified Data.Attoparsec.Text        as A
import qualified Data.ByteString.Char8       as B
import           Data.Foldable
import           Data.List                           (sort)
import qualified Data.Text.IO                as T.IO
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII

process1 dir fp = do
  bstr <- B.readFile (dir </> fp)
  let mptrs = decodeStrict bstr :: Maybe [PennTree]
  flip traverse_ mptrs $ \ptrs -> 
    mapM_ (T.IO.putStrLn . prettyPrint 0) ptrs



main1 = do
  let dir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710" 
  cnts <- getDirectoryContents dir

  let ptree_files = filter (\f -> takeExtensions f == ".corenlp_ptree") cnts

  mapM_ (process1 dir) (take 5 ptree_files)


parseOntoNotesPennTree :: FilePath -> IO (Either String [PennTree])
parseOntoNotesPennTree f = fmap (A.parseOnly (A.many1 (A.skipSpace *> pnode))) (T.IO.readFile f)


process basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
  -- print fps
  flip traverse_ (take 20 parsefiles) $ \f -> do
    putStrLn "\n\n\n=============================================================================================="
    print f
    etrs <- parseOntoNotesPennTree f
    -- print etr
    flip traverse_ etrs $ \trs -> 
      mapM_ (T.IO.putStrLn . prettyPrint 0) trs


main = do
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  process basedir  
