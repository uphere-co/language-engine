{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.STM
import           Control.Lens              ((^?),(^.),(^..), only )
import           Control.Monad             (join,when)
-- import           Control.Monad.Identity
import           Control.Monad.Loops       (whileJust_)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Foldable              (forM_)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (sort)
import           Data.List.Split           (chunksOf)
import           Data.Maybe                (fromJust,listToMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read            (decimal)
import           Data.Time.Clock
import           Data.Time.Format          (parseTimeM,defaultTimeLocale)
import           Options.Applicative
import           System.Console.Haskeline
import           System.FilePath
import           System.Directory
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Frame         (p_frame)
import qualified HFrameNet.Parse.LexUnit as LU (p_lexUnit)
import           HFrameNet.Query.Frame
import           HFrameNet.Type.Frame

import qualified HFrameNet.Type.LexUnit as LU (LexUnit)
  

data ProgOption = ProgOption { fnDataDir :: FilePath
                             , progCommand :: String
                             } deriving Show


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strArgument (help "command frame/lu")


progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "query program for FrameNet")


loadFrameData :: FilePath -> IO FrameDB
loadFrameData dir = do
  cnts <- getDirectoryContents dir
  let lst = (map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml")) cnts
  constructFrameDB lst
  

queryFrame :: FrameDB -> IO ()
queryFrame framemap = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
     let input = T.pack input'
         mfrm = HM.lookup input (framemap^.frameDB)
     case mfrm of
       Nothing -> putStrLn "no such frame"
       Just frm -> do
         TIO.putStrLn (frm^.frame_definition)
         TIO.putStrLn "============================"
         mapM_ printRelation (frm^.frame_frameRelation)
         TIO.putStrLn "============================\n\n"


printRelatedFrames :: [RelatedFrame] -> IO ()
printRelatedFrames rfrms = 
  let rfrmss = chunksOf 7 rfrms
  in mapM_ (TIO.putStrLn . T.intercalate "\t" . map (^.relframe_content)) rfrmss
    


printRelation :: FrameRelation -> IO ()
printRelation rel = 
  when ((not.null) (rel^.frel_relatedFrame)) $ do
    TIO.putStrLn (rel^.frel_type)
    TIO.putStrLn "----------------------------"
    printRelatedFrames (rel^.frel_relatedFrame)
    TIO.putStrLn "============================"


loadLUData :: FilePath -> IO ()
loadLUData dir = do
  cnts <- getDirectoryContents dir
  let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
  ref <- newTVarIO (0,HM.empty)
  forM_ (zip [1..] lst) $ \(i,fp) -> do
    when (i `mod` 100 == 0) $
      putStrLn (show i)
    forkIO $ processEachLU ref dir fp
  m <- atomically $ do
    (n,m) <- readTVar ref
    if n /= 0 then retry else return m
  print $ HM.lookup "lu16412.xml" m 


processEachLU :: TVar (Int,HM.HashMap FilePath LU.LexUnit) -> FilePath -> FilePath -> IO ()
processEachLU ref dir fp = do
  -- putStrLn fp
  atomically $ do
    (n,m) <- readTVar ref
    writeTVar ref (n+1,m)
  txt <- TLIO.readFile (dir </> fp)
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  case LU.p_lexUnit lu of
    Nothing -> error fp
    Just x -> do
      atomically $ do
        (n,m) <- readTVar ref
        let m' = HM.insert fp x m
        m' `seq` writeTVar ref (n-1,m')
      return () 


main :: IO ()
main = do
  opt <- execParser progOption
  case progCommand opt of
    "frame" -> loadFrameData (fnDataDir opt </> "frame") >>= queryFrame
    "lu" -> loadLUData (fnDataDir opt </> "lu") 
    o -> putStrLn ("cannot understand command " ++ o )
