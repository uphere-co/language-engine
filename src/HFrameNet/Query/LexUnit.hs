{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Query.LexUnit where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Async     (async,wait)
import           Control.Lens
import           Control.Monad                (when)
import           Data.Foldable                (forM_)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap          as IM
import           Data.List                    (foldl',sort)
import           Data.Text                    (Text)
import qualified Data.Text.Lazy.IO    as TLIO
import           System.Directory             (getDirectoryContents)
import           System.FilePath              ((</>),(<.>),takeExtensions)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.LexUnit      (p_lexUnit)
import           HFrameNet.Type.Common
import           HFrameNet.Type.LexUnit


newtype LexUnitDB = LexUnitDB { _lexunitDB :: IntMap LexUnit } 


makeLenses ''LexUnitDB


emptyDB = LexUnitDB IM.empty


processEachLU :: FilePath -> FilePath -> IO LexUnit
processEachLU dir fp = do
  txt <- TLIO.readFile (dir </> fp)
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  case p_lexUnit lu of
    Nothing -> error fp
    Just x -> return x


loadLUData :: FilePath -> IO LexUnitDB
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
  as <- flip mapM (zip [1..] lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    async (processEachLU dir fp)
  xs <- mapM wait as
    
  let lumap = foldl' (\m x -> (lexunitDB %~ IM.insert (x^.lexunit_basicLUAttributes.bluattr_ID) x) m) emptyDB xs
  return lumap 


queryLU db = do
  print $ IM.lookup 16412 (db^.lexunitDB)
