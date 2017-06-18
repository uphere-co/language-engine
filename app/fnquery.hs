{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid               ((<>))
import           Options.Applicative
import           System.FilePath
--
import           FrameNet.Query.Frame
import           FrameNet.Query.LexUnit

  
data ProgOption = ProgOption { fnDataDir :: FilePath
                             , progCommand :: String
                             } deriving Show


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strArgument (help "command frame/lu")


progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "query program for FrameNet")


main :: IO ()
main = do
  opt <- execParser progOption
  case progCommand opt of
    "frame" -> loadFrameData (fnDataDir opt </> "frame") >>= queryFrame
    "lu"    -> loadLUData (fnDataDir opt </> "lu") >>= queryLU
    o -> putStrLn ("cannot understand command " ++ o )
