{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens                            ((^.))
import           Data.Monoid                             ((<>))
import qualified Options.Applicative          as O
--
import           OntoNotes.App.Load                      (cfg,cfgG)
--
import           SRL.Analyze                             (runAnalysis)
import qualified SRL.Analyze.Config           as Analyze



pOptions :: O.Parser Analyze.Config
pOptions = Analyze.Config 
           <$> O.switch (O.long "detail" <> O.short 'd' <> O.help "Whether to show detail")
           <*> O.switch (O.long "full" <> O.short 'f' <> O.help "Whether to show full detail")
           <*> O.switch (O.long "config" <> O.short 'c' <> O.help "Whether to use global config")

progOption :: O.ParserInfo Analyze.Config
progOption = O.info pOptions (O.fullDesc <> O.progDesc "analyze text")


main :: IO ()
main = do
  acfg <- O.execParser progOption
  if (acfg ^. Analyze.useGlobalConfig)
    then runAnalysis cfgG acfg
    else runAnalysis cfg acfg
