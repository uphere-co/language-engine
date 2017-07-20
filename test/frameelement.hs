{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           System.FilePath
import           Text.Printf
--
import           FrameNet.Query.Frame
import           FrameNet.Type.Frame

main = do
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  FrameDB db <- loadFrameData (fndir </> "frame")

  let lst = map snd $ HM.toList db
      fes = map (\f -> f^..frame_FE
                          .traverse
                          .filtered (\r -> r^. fe_coreType == "Core-Unexpressed" )
                          .fe_name)
                          -- .to ((,) <$> view fe_ID <*> view fe_name))
                          -- .runGetter ((,) <$> Getter fe_ID <*> Getter fe_name))
                          lst
      fes' = map (\r -> (head r,length r)) . group . sort . concat $ fes
      
  mapM_ (\(fe,n) -> putStrLn (printf "%25s: %3d" fe n)) (sortBy (flip compare `on` snd) fes')
  
  -- print (length fes')
