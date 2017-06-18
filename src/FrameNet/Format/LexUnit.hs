{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Format.LexUnit where

import           Control.Lens
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Text.Printf
--
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit


formatLexeme :: Lexeme -> String
formatLexeme lx = printf "%20s %5s" (lx^.lexeme_name) (lx^.lexeme_POS)


printLexUnit :: LexUnit -> IO ()
printLexUnit lu = do
  TIO.putStrLn (lu^.lexunit_definition)
  TIO.putStrLn "============================"
  print (lu^.lexunit_basicLUAttributes)
  TIO.putStrLn "============================"
  print (lu^.lexunit_frameReference)
  TIO.putStrLn "============================"
  mapM_ (putStrLn . formatLexeme) (lu^.lexunit_lexeme)
