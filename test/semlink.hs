{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.List                 (sort)
import qualified Data.Text.Lazy.IO as T.L.IO
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath
--
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink


main :: IO ()
main = do
  let file= "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"

  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnmappingdata f of
                Left err -> error err
                Right c -> print c
