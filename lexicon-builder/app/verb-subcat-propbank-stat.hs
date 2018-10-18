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
import           Options.Applicative         hiding (str)
import           System.Directory.Tree
--
import           OntoNotes.Type.SenseInventory
--
import           Lexicon.Data
import           Lexicon.App.VerbSubcat


data ProgOption = ProgOption { _showDetail :: Bool
                             , _statOnly   :: Bool
                             , _tsvFormat  :: Bool
                             , _configFile :: FilePath
                             } deriving Show

makeLenses ''ProgOption

pOptions :: Parser ProgOption
pOptions = ProgOption <$> switch (long "detail" <> short 'd' <> help "Whether to show detail")
                      <*> switch (long "stat" <> short 's' <> help "Calculate statistics")
                      <*> switch (long "tsv" <> short 't' <> help "tsv format")
                      <*> strOption (long "config" <> short 'c' <> help "config file")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "PropBank statistics relevant to verb subcategorization")


main :: IO ()
main = do
  opt <- execParser progOption
  cfg  <- loadLexDataConfig (opt^.configFile) >>= \case Left e -> error e
                                                        Right x  -> return x
  sensedb <- HM.fromList . map (\si->(si^.inventory_lemma,si)) <$> loadSenseInventory (cfg^.cfg_sense_inventory_file)  
  
  dtr <- build (cfg^.cfg_wsj_directory)
  let fps = sort (toList (dirTree dtr))
  process cfg (opt^.statOnly,opt^.tsvFormat,opt^.showDetail) sensedb fps

