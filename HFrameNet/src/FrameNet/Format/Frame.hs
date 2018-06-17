{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Format.Frame where

import           Control.Lens
import           Control.Monad          (when)
import           Data.List.Split        (chunksOf)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
--
import           FrameNet.Type.Frame


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

