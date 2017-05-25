{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Monoid                ((<>))
import qualified Data.Text           as T
import qualified Options.Applicative as O
import           System.Console.Haskeline
--
import           PropBank

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "PropBank lookup")
  
main :: IO ()
main = do
  opt <- O.execParser progOption
  pdb <- constructPredicateDB <$> constructFrameDB (dir opt)
  let rdb = constructRoleSetDB pdb
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ 
    let input = T.pack input'
    in case T.split (== '.') input of
         (_x:_n:_) -> queryRoleSet rdb input
         (_x:[])  -> queryPredicate pdb input
         [] -> putStrLn "query is not recognized."
