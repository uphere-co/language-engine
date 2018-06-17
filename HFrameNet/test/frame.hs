{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.Split
import           Data.Maybe                  (fromMaybe,listToMaybe,maybeToList)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
import qualified Data.Text.Lazy      as T.L
import           System.FilePath
import           Text.Printf
--
import           FrameNet.Query.Frame
import           FrameNet.Type.Common
import           FrameNet.Type.Definition
import           FrameNet.Type.Frame

format xs = intercalate "\t" (map (printf "%-30s") xs)

frameType f = printf "%-30s: %s" (f^.frame_name) (T.intercalate ", " (f^..frame_semType.traverse.semtype_name))


fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"

countFrame = do
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


showDefinition = do
  FrameDB db <- loadFrameData (fndir </> "frame")
  let txt = fromMaybe "" ((^.frame_definition) <$> HM.lookup "Purpose" db)
      defn = p_defRoot (T.L.fromStrict txt)
  T.IO.putStrLn txt
  print defn

main = showDefinition
