{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.List                 (sort)
import qualified Data.Text.Lazy.IO as T.L.IO
import           System.Directory
import           System.FilePath
import           Text.Taggy.Lens
--
import           OntoNotes.Parser.SenseInventory

main :: IO ()
main = do
  let dir= "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts) 
  -- let fs = ["stock-v.xml"] -- [ "fracture-v.xml" ] -- [ "get-v.xml" ]
  flip mapM_ fs $ \f -> do
    let fp = dir  </> f
    print fp
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just f -> case p_inventory f of
                  Left err -> error err
                  Right c  -> return () -- print c


