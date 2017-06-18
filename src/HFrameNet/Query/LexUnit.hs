{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Query.LexUnit where

import           Control.Concurrent           (forkIO)
-- import           Control.Concurrent.STM       (TVar(..),atomically,newTVarIO,readTVar,retry,writeTVar)
import           Control.Concurrent.Async     (async,wait)
import           Control.Lens
import           Control.Monad                (when)
import           Data.Foldable                (forM_)
-- import           Data.HashMap.Strict          (HashMap)
-- import qualified Data.HashMap.Strict  as HM
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


processEachLU :: {- TVar (Int,LexUnitDB) -> -} FilePath -> FilePath -> IO LexUnit
processEachLU {- ref -} dir fp = do
  {- atomically $ do
    (n,m) <- readTVar ref
    writeTVar ref (n+1,m) -}
  txt <- TLIO.readFile (dir </> fp)
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  case p_lexUnit lu of
    Nothing -> error fp
    Just x -> return x
      {- 
      atomically $ do
        (n,m) <- readTVar ref
        let m' = (lexunitDB %~ IM.insert (x^.lexunit_basicLUAttributes.bluattr_ID) x) m
        m' `seq` writeTVar ref (n-1,m')
      return ()  -}


loadLUData :: FilePath -> IO LexUnitDB
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
  -- ref <- newTVarIO (0,emptyDB)
  as <- flip mapM (zip [1..] lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    async (processEachLU {- ref -} dir fp)

  xs <- mapM wait as
    

  let lumap = foldl' (\m x -> (lexunitDB %~ IM.insert (x^.lexunit_basicLUAttributes.bluattr_ID) x) m) emptyDB xs
  return lumap 
{- 
  return ( xs) 
  mapM_ 
  atomically $ do
    (n,m) <- readTVar ref
    if n /= 0 then retry else return m

-}

queryLU db = do
  print $ IM.lookup 16412 (db^.lexunitDB)
