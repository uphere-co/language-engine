{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Binary
import           Data.Monoid
import           Options.Applicative
--
import           WordNet.Query.SynsetDB


data ProgOption = ProgOption { dir :: FilePath
                             , isTesting :: Bool
                             , fileName :: FilePath
                             } deriving Show


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> switch (long "test" <> short 't' <> help "testing")
                      <*> strArgument (metavar "FILENAME" <> help "filename")
  

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "WordNet lexicographer encode/decode")


main :: IO ()
main = do
  opt <- execParser progOption
  if isTesting opt
    then do
      lbstr <- BL.readFile (fileName opt)
      let db = decode lbstr :: SynsetDB
      case lookup "noun.food" (db^.synsetdb_noun)  of
        Nothing -> error "Nothing"
        Just xs -> mapM_ print (take 10 (reverse xs))
      case lookup "verb.motion" (db^.synsetdb_verb) of
        Nothing -> error "Nothing"
        Just xs -> mapM_ print (take 10 (reverse xs))
      case lookup "adj.all" (db^.synsetdb_adjective) of
        Nothing -> error "Nothing"
        Just xs -> mapM_ print (take 10 (reverse xs))
      case lookup "adv.all" (db^.synsetdb_adverb) of
        Nothing -> error "Nothing"
        Just xs -> mapM_ print (take 10 (reverse xs))
    else do
      m <- processAll (dir opt)
      BL.writeFile (fileName opt) $ encode m      

