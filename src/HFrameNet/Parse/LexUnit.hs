{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Parse.LexUnit where

import           Control.Applicative
import           Control.Lens ((^?),(^.),(^..),_Just,only)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Common
import           HFrameNet.Type.LexUnit
import           HFrameNet.Util


p_lexUnit :: Element -> Maybe LexUnit
p_lexUnit x = LexUnit <$> (p_header =<< getOnly1 x "header")
                      <*> getOnly1 x "definition" ^? _Just.element.contents
                      <*> mapM p_lexeme (getOnly x "lexeme")
                      <*> (readDecimal =<< x ^. attr "totalAnnotated")


p_header :: Element -> Maybe Header
p_header x = Header <$> mapM p_corpus (getOnly x "corpus")

p_corpus :: Element -> Maybe Corpus
p_corpus x = Corpus <$> mapM p_document (getOnly x "document")


p_document :: Element -> Maybe Document
p_document x = Document <$> (readDecimal =<< x ^. attr "ID")
                        <*> x ^. attr "name"
                        <*> x ^. attr "description"

