module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap         as IM
import           Data.List
import           System.Directory
import           System.FilePath
--
import           FrameNet.Query.LexUnit

{- 
load fndir = do
  cnts <- getDirectoryContents dir
  let lst = map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml") . sort $ cnts
  as <- flip mapM (zip ([1..] :: [Int]) lst) $ \(i,fp) -> do
    -- when (i `mod` 1000 == 0) $
    putStrLn (show i)
    async (parseLUFile fp)
  xs <- mapM wait as
    
  let lumap = foldl' insertLU emptyDB xs

  return lumap 
  -}


main :: IO ()
main = do
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  LexUnitDB lexdb _ <- loadLUData (fndir </> "lu")
  
  print (IM.lookup 4748 lexdb)  
