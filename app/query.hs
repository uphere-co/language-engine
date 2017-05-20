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

queryRoleSet db input = do
  print (HM.lookup input (db^.rolesetDB))

  
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
