{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.Split
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
import           System.FilePath
import           Text.Printf
--
import           FrameNet.Query.Frame
import           FrameNet.Type.Common
import           FrameNet.Type.Frame

format xs = intercalate "\t" (map (printf "%-30s") xs)

frameType f = printf "%-30s: %s" (f^.frame_name) (T.intercalate ", " (f^..frame_semType.traverse.semtype_name))

main = do
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  FrameDB db <- loadFrameData (fndir </> "frame")

  let lst = sortBy (compare `on` (^.frame_name)) . map snd . HM.toList $ db
  mapM_ (putStrLn . frameType) lst
  let nonlexicals = filter (\f -> "Non-Lexical Frame" `elem` (f^..frame_semType.traverse.semtype_name)) lst
  -- mapM_ (\f -> print (f^.frame_name)) nonlexicals
  putStrLn "---------------------------------------------------------------"
  putStrLn $ "number of non-lexical frames: " ++ show (length nonlexicals)
  {-     lsts = chunksOf 5 lst
  mapM (putStrLn . format) lsts
  -}
