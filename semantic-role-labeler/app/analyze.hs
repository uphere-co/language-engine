{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Lens                            ((^.))
import           Data.Aeson                              (eitherDecode')
import qualified Data.ByteString.Lazy         as BL
import           Data.Monoid                             ((<>))
import qualified Options.Applicative          as O
--
import           SRL.Analyze                             (runAnalysis)
import           SRL.Analyze.Config                      (SRLConfig)
import qualified SRL.Analyze.Config           as Analyze



pOptions :: O.Parser Analyze.Config
pOptions = Analyze.Config
           <$> O.switch (O.long "detail" <> O.short 'd' <> O.help "Whether to show detail")
           <*> O.switch (O.long "full" <> O.short 'f' <> O.help "Whether to show full detail")
           <*> O.switch (O.long "bypassWIKINER" <> O.short 'n' <> O.help "Whether to bypass wiki-ner")
           <*> O.switch (O.long "bypassTEXTNER" <> O.short 'm' <> O.help "Whether to bypass text NER")
           <*> O.strOption (O.long "config" <> O.short 'c' <> O.help "config file")


progOption :: O.ParserInfo Analyze.Config
progOption = O.info pOptions (O.fullDesc <> O.progDesc "analyze text")


main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg  <-do e <- eitherDecode' @SRLConfig <$> BL.readFile (acfg ^. Analyze.configFile)
            case e of
              Left err -> error err
              Right x -> return x
  runAnalysis cfg acfg
