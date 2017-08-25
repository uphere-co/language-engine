{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens                            ((^.),makeLenses)
import           Data.Monoid                             ((<>))
import qualified Options.Applicative          as O
--
import           OntoNotes.App.Analyze                   (runAnalysis)
import qualified OntoNotes.App.Analyze.Config as Analyze
import           OntoNotes.App.Load                      (cfg,cfgG)



data ProgOption = ProgOption { _showDetail :: Bool
                             }
                deriving Show

makeLenses ''ProgOption

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.switch (O.long "detail" <> O.short 'd' <> O.help "Whether to show detail")


progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "analyze text")


main :: IO ()
main = do
  opt <- O.execParser progOption
  let acfg = Analyze.Config (opt^.showDetail)
  runAnalysis cfg acfg
