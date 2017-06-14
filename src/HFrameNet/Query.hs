{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Query where

import           Control.Lens
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Text                    (Text)
import qualified Data.Text.Lazy.IO    as TLIO
import           System.FilePath              ((</>),(<.>))
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Frame
import           HFrameNet.Type.Frame

newtype FrameDB = FrameDB { _frameDB :: HashMap Text Frame } 

makeLenses ''FrameDB

constructFrameDB :: [FilePath] -> IO FrameDB
constructFrameDB fps = do
   frames <- mapM parseFrameFile fps
   return $ FrameDB (HM.fromList (map (\f->(f^.frame_name,f)) frames))
  
parseFrameFile :: FilePath -> IO Frame
parseFrameFile fp = do
  -- putStrLn fp
  txt <- TLIO.readFile fp
  let frame = head (txt ^.. (html . allNamed (only "frame")))
  case p_frame frame of
    Nothing -> error fp
    Just x -> return x
{-       print x
      return () -}

