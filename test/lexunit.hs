module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           System.Directory
import           System.FilePath
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.LexUnit

-- insertLUinList :: 

load :: FilePath -> IO [LexUnit] 
load dir = do
  cnts <- getDirectoryContents dir
  let lst = map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml") . sort $ cnts
  as <- flip mapM (zip ([1..] :: [Int]) lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    async (parseLUFile fp)
  xs <- mapM wait as
  return xs
  -- let lumap = foldl' insertLU emptyDB xs
  -- return lumap 



main' :: IO ()
main' = do
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  lst <- load (fndir </> "lu")
  BL.writeFile "test.dat" (encode lst)
  -- print (IM.lookup 4748 lexdb)  


main :: IO ()
main = do
  bstr <- BL.readFile "test.dat"
  let lst = decode bstr :: [LexUnit]
      LexUnitDB ldb _ = foldl' insertLU emptyDB lst
  print (IM.lookup 4748 ldb)
