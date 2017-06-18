{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Query.LexUnit where

import           Control.Concurrent.Async     (async,wait)
import           Control.Lens
import           Control.Monad                (when)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap          as IM
import           Data.List                    (foldl',sort)
import qualified Data.Text.Lazy.IO    as TLIO
import           System.Directory             (getDirectoryContents)
import           System.FilePath              ((</>),takeExtensions)
import           Text.Taggy.Lens
--
import           FrameNet.Parse.LexUnit      (p_lexUnit)
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit


newtype LexUnitDB = LexUnitDB { _lexunitDB :: IntMap LexUnit } 


makeLenses ''LexUnitDB


emptyDB :: LexUnitDB
emptyDB = LexUnitDB IM.empty


parseLUFile :: FilePath -> IO LexUnit
parseLUFile fp = do
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  case p_lexUnit lu of
    Nothing -> error fp
    Just x -> return x


loadLUData :: FilePath -> IO LexUnitDB
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml") . sort $ cnts
  as <- flip mapM (zip ([1..] :: [Int]) lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    async (parseLUFile fp)
  xs <- mapM wait as
    
  let lumap = foldl' (\m x -> (lexunitDB %~ IM.insert (x^.lexunit_basicLUAttributes.bluattr_ID) x) m) emptyDB xs
  return lumap 


queryLU :: LexUnitDB -> IO ()
queryLU db = do
  print $ IM.lookup 16412 (db^.lexunitDB)
