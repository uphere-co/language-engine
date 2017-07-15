module Main where

import           Control.Lens
import qualified Data.HashMap.Strict as HM
import           System.FilePath
--
import FrameNet.Query.Frame
import FrameNet.Type.Frame

main = do
  putStrLn "ha"
  let fndir = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7"
  FrameDB db <- loadFrameData (fndir </> "frame")

  let lst = map snd $ HM.toList db
      fes = map (\f -> f^..frame_FE.traverse
                          .runGetter ((,) <$> Getter fe_ID <*> Getter fe_name)) lst
  mapM_ print fes
