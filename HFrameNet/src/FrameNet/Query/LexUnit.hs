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
import           Text.Taggy.Lens       hiding (name)
--
import           FrameNet.Format.LexUnit      (printLexUnit)
import           FrameNet.Parser.LexUnit      (p_lexUnit)
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit


data LexUnitDB = LexUnitDB { _lexunitDB :: IntMap LexUnit
                           , _nameIDmap :: HashMap Text [Int]
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
  in  (lexunitDB %~ IM.insert i lu) . (nameIDmap %~ HM.insertWith (++) n [i]) $ db 


loadLUData :: FilePath -> IO LexUnitDB
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml") . sort $ cnts
  as <- flip mapM (zip ([1..] :: [Int]) lst) $ \(i,fp) -> do
    when (i `mod` 1000 == 0) $
      putStrLn (show i)
    async (parseLUFile fp)
  xs <- mapM wait as
  return (foldl' insertLU emptyDB xs)



queryLU :: LexUnitDB -> IO ()
queryLU db = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        ws = T.words input
    case ws of
      "id":idstr:_ -> 
        case decimal idstr of
          Left err -> putStrLn err
          Right (i,_) ->
            let mlu = IM.lookup i (db^.lexunitDB)
            in case mlu of
                 Nothing -> putStrLn "no such lexical unit"
                 Just lu -> printLexUnit lu
      "name":name:_ -> 
        let mlu = HM.lookup name (db^.nameIDmap) >>= \is -> mapM (flip IM.lookup (db^.lexunitDB)) is
        in case mlu of
             Nothing -> putStrLn "no such lexical unit"
             Just lus -> mapM_ printLexUnit lus
      _ -> putStrLn "cannot understand" 
               
