{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Query.Frame where

import           Control.Lens
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (whileJust_)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List.Split              (chunksOf)
import           Data.Text                    (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy.IO    as TLIO
import           System.Console.Haskeline
import           System.FilePath              ((</>),(<.>),takeExtensions)
import           System.Directory             (getDirectoryContents)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Frame
import           HFrameNet.Type.Frame

newtype FrameDB = FrameDB { _frameDB :: HashMap Text Frame } 

makeLenses ''FrameDB

  
parseFrameFile :: FilePath -> IO Frame
parseFrameFile fp = do
  txt <- TLIO.readFile fp
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  case p_frame frame of
    Nothing -> error fp
    Just x -> return x


constructFrameDB :: [FilePath] -> IO FrameDB
constructFrameDB fps = do
   frames <- mapM parseFrameFile fps
   return $ FrameDB (HM.fromList (map (\f->(f^.frame_name,f)) frames))


loadFrameData :: FilePath -> IO FrameDB
loadFrameData dir = do
  cnts <- getDirectoryContents dir
  let lst = (map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml")) cnts
  constructFrameDB lst


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

