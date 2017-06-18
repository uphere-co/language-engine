{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Format.LexUnit where

import           Control.Lens
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Text.Printf
--
import           FrameNet.Type.Common
import           FrameNet.Type.LexUnit
import           FrameNet.Type.Sentence


formatAttrib :: BasicLUAttributes -> String
formatAttrib attr = printf "%7s %7d\n%7s %20s\n%7s %5s"
                      ("id:"   :: Text) (attr^.bluattr_ID)
                      ("name:" :: Text) (attr^.bluattr_name)
                      ("POS:"  :: Text) (show (attr^.bluattr_POS))

formatFrameReference :: FrameReference -> String
formatFrameReference fr = printf "%7s %20s" ("frame:" :: Text) (fromMaybe "" (fr^.fr_frame))
                        

formatLexeme :: Lexeme -> String
formatLexeme lx = printf "%7s %18s %s" ("lexeme:" :: Text) (lx^.lexeme_name) (lx^.lexeme_POS)


formatSentence :: Sentence -> String
formatSentence sent = printf "- %s" (sent^.sent_text)


printLexUnit :: LexUnit -> IO ()
printLexUnit lu = do
  TIO.putStrLn "============================"
  putStrLn $ formatAttrib (lu^.lexunit_basicLUAttributes)
  putStrLn $ formatFrameReference (lu^.lexunit_frameReference)
  mapM_ (putStrLn . formatLexeme) (lu^.lexunit_lexeme)
  TIO.putStrLn ("definition: " <> lu^.lexunit_definition)
  TIO.putStrLn "---- example sentences -----"  
  mapM_ (putStrLn . formatSentence) . take 3 $ (lu^..lexunit_subCorpus.traverse.subcorp_sentence.traverse)
  TIO.putStrLn "============================"
