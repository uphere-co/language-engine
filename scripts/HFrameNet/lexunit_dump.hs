module Main where

import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text.IO               as T.IO
import           System.Directory
import           System.FilePath
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit

{- 
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
-}


{- 
main' :: IO ()
main' = do
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  lst <- load (fndir </> "lu")
  BL.writeFile "test.dat" (encode lst)
  -- print (IM.lookup 4748 lexdb)  
-}

main :: IO ()
main = do
  bstr <- BL.readFile "test.dat"
  let lst = decode bstr :: [LexUnit]
      -- idxs = map (^.lexunit_basicLUAttributes.bluattr_ID) lst
      -- LexUnitDB ldb _ = foldl' insertLU emptyDB lst
      -- mresults = traverse (flip IM.lookup ldb) [4748,4727]
  -- print idxs
  {-     
  case mresults of
    Nothing -> print "lookup failed"
    Just results -> do -}
  do
      let extractPTfromPattern patt = do vu <- maybeToList (patt^.patt_valenceUnit)
                                         return (vu^.vu_PT)
          pts :: [Text]
          pts = do lu <- lst -- results
                   v <- maybeToList (lu^.lexunit_valences)
                   patt <- (v^..val_FERealization.traverse.fereal_pattern.traverse) ++
                           (v^..val_FEGroupRealization.traverse.fegroup_pattern.traverse)
                   extractPTfromPattern patt

          fevals :: [Text]
          fevals = do lu <- lst
                      v <- maybeToList (lu^.lexunit_valences)
                      fereal <- (v^..val_FERealization.traverse)
                      feval <- maybeToList (fereal^.fereal_FE)
                      return (feval ^.feval_name)
{-           gpts :: [Text]
          gtps = do lu <- lst
                    v <- maybeToList (lu^.lexunit_valences)
                    patt
-}
                    
          pts' = (map head . group . sort) pts 
      -- mapM_ (T.IO.putStrLn) pts'
      mapM_ (T.IO.putStrLn) fevals
