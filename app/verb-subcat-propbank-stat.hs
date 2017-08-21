{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict          as HM
import           Data.List                          (sort)
import           Data.Monoid
import           Options.Applicative         hiding (str)
import           System.Directory.Tree
--
import           OntoNotes.App.Load
import           OntoNotes.App.VerbSubcat
import           OntoNotes.Type.SenseInventory


data ProgOption = ProgOption { showDetail :: Bool
                             , statOnly   :: Bool
                             , tsvFormat  :: Bool
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "detail" <> short 'd' <> help "Whether to show detail")
                      <*> switch (long "stat" <> short 's' <> help "Calculate statistics")
                      <*> switch (long "tsv" <> short 't' <> help "tsv format")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "PropBank statistics relevant to verb subcategorization")


main :: IO ()
main = do
  opt <- execParser progOption
  sensedb <- HM.fromList . map (\si->(si^.inventory_lemma,si)) <$> loadSenseInventory (cfg^.cfg_sense_inventory_file)  
  
  dtr <- build (cfg^.cfg_wsj_directory)
  let fps = {- filter (\f -> takeBaseName f == "wsj_2446") . Prelude.take 200 $ -}
            sort (toList (dirTree dtr))
  process (statOnly opt,tsvFormat opt,showDetail opt) sensedb fps

