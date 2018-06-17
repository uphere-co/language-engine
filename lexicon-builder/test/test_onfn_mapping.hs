{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
--
import           FrameNet.Query.Frame
import           Lexicon.Mapping.OntoNotesFrameNet
import           Lexicon.Data
import           Lexicon.Type


main :: IO ()
main = do
  cfg <- loadLexDataConfig "config.json.mark" >>= \case Left err -> error err
                                                        Right x  -> return x
  fdb <- loadFrameData (cfg^.cfg_framenet_framedir)

  let xs = do (w,lst) <- mapFromONtoFN
              (m,t) <- lst
              let mfr = HM.lookup (unFNFrame t) (fdb^.frameDB)
              guard (isNothing mfr)
              return (w,m,t)
  -- mapM_ print . filter (\(_,_,t) -> isJust (T.find (\c -> c == ' ' || c == '(') t)) $  xs

  mapM_ print xs
