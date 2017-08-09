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


loadVNFN :: IO ()
loadVNFN = do
  let file= "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"

  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnmap f of
                Left err -> error err
                Right c -> print c


loadVNFNRole :: IO ()
loadVNFNRole = do
  let file= "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VN-FNRoleMapping.txt"

  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnetRoles-framenetFEs_RoleMappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnrolemap f of
                Left err -> error err
                Right c -> print c



loadPBVN :: IO ()
loadPBVN = do
  let file= "/scratch/wavewave/SemLink/1.2.2c/vn-pb/vnpbMappings"

  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "pbvn-typemap") of
    Nothing -> error "nothing"
    Just f -> case p_pbvnmap f of
                Left err -> error err
                Right c -> print c
