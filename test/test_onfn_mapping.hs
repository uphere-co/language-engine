module Main where

import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
--
import           FrameNet.Query.Frame
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.App.Load


main :: IO ()
main = do
  fdb <- loadFrameData (cfg^.cfg_framenet_framedir)

  let xs = do (w,lst) <- mapFromONtoFN
              (m,t) <- lst
              let mfr = HM.lookup t (fdb^.frameDB)
              guard (isNothing mfr)
              return (w,m,t)
  -- mapM_ print . filter (\(_,_,t) -> isJust (T.find (\c -> c == ' ' || c == '(') t)) $  xs

  mapM_ print xs
