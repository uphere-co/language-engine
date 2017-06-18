{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Query.LexUnit where

import           Control.Concurrent.Async     (async,wait)
import           Control.Lens
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (whileJust_)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap          as IM
import           Data.List                    (foldl',sort)
import           Data.Text                    (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy.IO    as TLIO
import           Data.Text.Read
import           System.Console.Haskeline
import           System.Directory             (getDirectoryContents)
import           System.FilePath              ((</>),takeExtensions)
import           Text.Taggy.Lens
--
import           FrameNet.Parse.LexUnit      (p_lexUnit)
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit


data LexUnitDB = LexUnitDB { _lexunitDB :: IntMap LexUnit
                           , _nameIDmap :: HashMap Text Int
                           } 


makeLenses ''LexUnitDB


emptyDB :: LexUnitDB
emptyDB = LexUnitDB IM.empty HM.empty


parseLUFile :: FilePath -> IO LexUnit
parseLUFile fp = do
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  case p_lexUnit lu of
    Nothing -> error fp
    Just x -> return x


insertLU :: LexUnitDB -> LexUnit -> LexUnitDB
insertLU !db lu =
  let i = lu^.lexunit_basicLUAttributes.bluattr_ID
      n = lu^.lexunit_basicLUAttributes.bluattr_name
  in  (lexunitDB %~ IM.insert i lu) . (nameIDmap %~ HM.insert n i) $ db 


loadLUData :: FilePath -> IO LexUnitDB
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml") . sort $ cnts
  as <- flip mapM (zip ([1..] :: [Int]) lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    async (parseLUFile fp)
  xs <- mapM wait as
    
  let lumap = foldl' insertLU emptyDB xs

  return lumap 


queryLU :: LexUnitDB -> IO ()
queryLU db = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        mlu = HM.lookup input (db^.nameIDmap) >>= \i -> IM.lookup i (db^.lexunitDB)
    case mlu of
      Nothing -> putStrLn "no such lexical unit"
      Just lu -> print lu
        
