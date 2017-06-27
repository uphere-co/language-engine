module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative
--
import           WordNet.Query.SynsetDB
import           WordNet.Type.Lexicographer

data ProgOption = ProgOption { dir :: FilePath
                             , isTesting :: Bool
                             , fileName :: FilePath
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> switch (long "test" <> short 't' <> help "testing")
                      <*> strArgument (help "filename")
  

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "WordNet lexicographer encode/decode")


main :: IO ()
main = do
  opt <- execParser progOption
  if isTesting opt
    then do
      lbstr <- BL.readFile (fileName opt)
      let xs = decode lbstr :: [Either Synset SynsetCluster]
      mapM_ print (take 10 (reverse xs))
    else do
      er <- processAdjAll (dir opt)
      case er of
        Left err -> print err
        Right xs -> BL.writeFile (fileName opt) $ encode $ catMaybes xs

