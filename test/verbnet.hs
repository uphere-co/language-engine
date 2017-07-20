{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.List                 (sort)
import qualified Data.Text.Lazy.IO as T.L.IO
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath
--
import           VerbNet.Parser
import           VerbNet.Type


main :: IO ()
main = do
  let dir= "/scratch/wavewave/VerbNet/verbnet"
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts) 
  -- let fs = [ "admit-65.xml" ] -- [ "get-13.5.1.xml" ]
  flip mapM_ fs $ \f -> do
    let fp = dir  </> f
    print fp
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "VNCLASS") of
      Nothing -> error "nothing"
      Just f -> case p_vnclass f of
                  Left err -> error err
                  Right c  -> print c

