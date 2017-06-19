{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Parse.Frame where

import           Control.Applicative    ((<$>),(<*>))
import           Control.Lens           ((^?),(^.),(^..),_Just,only)
import           Text.Taggy.Lens
--
import           FrameNet.Parse.Common
import           FrameNet.Type.Frame
import           FrameNet.Util


p_frame :: Element -> Maybe Frame
p_frame x = Frame <$> (readDecimal =<< (x ^. attr "ID"))
                  <*> x ^. attr "name"
                  <*> (readTime =<< (x ^. attr "cDate"))
                  <*> getOnly1 x "definition" ^? _Just.element.contents
                  <*> mapM p_FE (x ^.. elements . named (only "FE"))
                  <*> mapM p_FEcoreSet (x ^.. elements . named (only "FEcoreSet"))
                  <*> mapM p_frameRelation (x ^.. elements . named (only "frameRelation"))
                  <*> mapM p_lexUnit (x ^.. elements . named (only "lexUnit"))


p_FE :: Element -> Maybe FE
p_FE x = FE <$> (readDecimal =<< (x ^. attr "ID"))
            <*> x ^. attr "name"
            <*> x ^. attr "abbrev"
            <*> (readTime =<< (x ^. attr "cDate"))
            <*> x ^. attr "coreType"
            <*> x ^. attr "fgColor"
            <*> x ^. attr "bgColor"
            <*> getOnly1 x "definition" ^? _Just.element.contents
            <*> mapM p_semType (getOnly x "semType")

p_FEcoreSet :: Element -> Maybe FEcoreSet
p_FEcoreSet x = FEcoreSet <$> mapM p_memberFE (getOnly x "memberFE")

p_memberFE :: Element -> Maybe MemberFE
p_memberFE x = MemberFE <$> (readDecimal =<< (x ^. attr "ID"))
                        <*> x ^. attr "name"

p_frameRelation :: Element -> Maybe FrameRelation
p_frameRelation x = FrameRelation <$> x ^. attr "type"
                                  <*> mapM p_relatedFrame (getOnly x "relatedFrame")

p_relatedFrame :: Element -> Maybe RelatedFrame
p_relatedFrame x = RelatedFrame <$> (readDecimal =<< (x ^. attr "ID"))
                                <*> pure (x ^. element . contents)



p_lexUnit :: Element -> Maybe LexUnit
p_lexUnit x = LexUnit <$> (readDecimal =<< (x ^. attr "ID"))
                      <*> x ^. attr "name"
                      <*> x ^. attr "POS"
                      <*> x ^. attr "status"
                      <*> (readTime =<< (x ^. attr "cDate"))
                      <*> x ^. attr "cBy"
                      <*> (readDecimal =<< (x ^. attr "lemmaID"))
                      <*> getOnly1 x "definition" ^? _Just.element.contents
                      <*> (p_sentenceCount =<< getOnly1 x "sentenceCount")
                      <*> mapM p_lexeme  (getOnly x "lexeme")
                      <*> mapM p_semType (getOnly x "semType")

p_sentenceCount :: Element -> Maybe SentenceCount
p_sentenceCount x = SentenceCount <$> (readDecimal =<< (x ^. attr "total"))
                                  <*> (readDecimal =<< (x ^. attr "annotated"))
  




