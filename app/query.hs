{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Function              (on)
import qualified Data.HashMap.Strict as HM
import           Data.List                  (sortBy)
import           Data.Maybe                 (catMaybes,fromMaybe,maybeToList)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB  (toLazyText)
import qualified Data.Text.Lazy.IO          as TLIO
import           Data.Text.Read             (decimal)
import qualified Options.Applicative as O
import           System.Console.Haskeline
import           System.FilePath            ((</>))
--
import           YAML.Builder
--
import           PropBank

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "PropBank lookup")
  
main = do
  opt <- O.execParser progOption
  pdb <- constructPredicateDB <$> constructFrameDB (dir opt)
  let rdb = constructRoleSetDB pdb
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ 
    let input = T.pack input'
    in case T.split (== '.') input of
         (x:n:_) -> queryRoleSet rdb input
         (x:[])  -> queryPredicate pdb input
         [] -> putStrLn "query is not recognized."
