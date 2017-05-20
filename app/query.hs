{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Text.Read             (decimal)
import qualified Options.Applicative as O
import           System.Console.Haskeline
import           System.FilePath            ((</>))
--
import           PropBank

data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "PropBank lookup")

queryPredicate db input = do
  print (HM.lookup input (db^.predicateDB))
  
main = do
  opt <- O.execParser progOption
  db <- constructPredicateDBFromFrameDB <$> constructFrameDB (dir opt)
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input -> liftIO $
    queryPredicate db (T.pack input) 
