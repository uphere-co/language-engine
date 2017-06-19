{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Parse.LexUnit where

import           Control.Applicative
import           Control.Lens             ((^?),(^.),_Just)
import           Data.Text                (Text)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Common
import           HFrameNet.Parse.Sentence
import           HFrameNet.Type.Common
import           HFrameNet.Type.LexUnit
import           HFrameNet.Util


p_lexUnit :: Element -> Maybe LexUnit
p_lexUnit x = LexUnit <$> (p_header =<< getOnly1 x "header")
                      <*> getOnly1 x "definition" ^? _Just.element.contents
                      <*> mapM p_lexeme (getOnly x "lexeme")
                      <*> mapM p_semType (getOnly x "semType")
                      <*> optional (p_valences =<< getOnly1 x "valences")
                      <*> mapM p_subCorpus (getOnly x "subCorpus")
                      <*> p_basicLUAttributes x
                      <*> p_frameReference x
                      <*> (readDecimal =<< x ^. attr "totalAnnotated")

p_basicLUAttributes :: Element -> Maybe BasicLUAttributes
p_basicLUAttributes x =
  BasicLUAttributes <$> (readDecimal =<< x ^. attr "ID")
                    <*> x ^. attr "name"
                    <*> (p_POS =<< x ^. attr "POS")
                    <*> optional (x ^. attr "incorporatedFE")
                    <*> optional (x ^. attr "status")

p_POS :: Text -> Maybe POS
p_POS "A"   = Just A
p_POS "ADV" = Just ADV
p_POS "ART" = Just ART
p_POS "AVP" = Just AVP
p_POS "C"   = Just C
p_POS "CCON" = Just CCON
p_POS "IDIO" = Just IDIO
p_POS "INTJ" = Just INTJ
p_POS "N"    = Just N
p_POS "NUM"  = Just NUM
p_POS "PREP" = Just PREP
p_POS "PRON" = Just PRON
p_POS "SCON" = Just SCON
p_POS "V"    = Just V
p_POS _      = Nothing

p_frameReference :: Element -> Maybe FrameReference
p_frameReference x
  = FrameReference <$> optional (readDecimal =<< x ^. attr "frameID")
                   <*> optional (x ^. attr "frame")


p_valences :: Element -> Maybe Valences
p_valences x = Valences <$> mapM p_governor (getOnly x "governor")
                        <*> mapM p_FERealization (getOnly x "FERealization")
                        <*> mapM p_FEGroupRealization (getOnly x "FEGroupRealization")

p_subCorpus :: Element -> Maybe SubCorpus
p_subCorpus x = SubCorpus <$> mapM p_sentence (getOnly x "sentence")
                          <*> x ^. attr "name"

p_governor :: Element -> Maybe Governor
p_governor x = Governor <$> mapM p_annoSet (getOnly x "annoSet")
                        <*> x ^. attr "lemma"
                        <*> x ^. attr "type"


p_FERealization :: Element -> Maybe FERealization
p_FERealization x = FERealization <$> optional (p_FEValence =<< getOnly1 x "FEValence")
                                  <*> mapM p_pattern (getOnly x "pattern")
                                  <*> (readDecimal =<< x ^. attr "total")

p_FEGroupRealization :: Element -> Maybe FEGroupRealization
p_FEGroupRealization x = FEGroupRealization <$> mapM p_FEValence (getOnly x "FEValence")
                                            <*> mapM p_pattern (getOnly x "pattern")
                                            <*> (readDecimal =<< x ^. attr "total")


p_FEValence :: Element -> Maybe FEValence
p_FEValence x = FEValence <$> x ^. attr "name"

p_pattern :: Element -> Maybe Pattern
p_pattern x = Pattern <$> optional (p_valenceUnit =<< getOnly1 x "valenceUnit")
                      <*> mapM p_annoSet (getOnly x "annoSet")
                      <*> (readDecimal =<< x ^. attr "total")

p_valenceUnit :: Element -> Maybe ValenceUnit
p_valenceUnit x = ValenceUnit <$> x ^. attr "FE"
                              <*> x ^. attr "PT"
                              <*> x ^. attr "GF"

p_annoSet :: Element -> Maybe AnnoSet
p_annoSet x = AnnoSet <$> (readDecimal =<< x ^. attr "ID")

p_header :: Element -> Maybe Header
p_header x = Header <$> mapM p_corpus (getOnly x "corpus")

p_corpus :: Element -> Maybe Corpus
p_corpus x = Corpus <$> mapM p_document (getOnly x "document")


p_document :: Element -> Maybe Document
p_document x = Document <$> (readDecimal =<< x ^. attr "ID")
                        <*> x ^. attr "name"
                        <*> x ^. attr "description"

