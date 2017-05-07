{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Parse.Sentence where

import           Control.Applicative
import           Control.Lens            ((^?),(^.),(^..),_Just,only)
import           Data.Text               (Text)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Common
import           HFrameNet.Type.Sentence
import           HFrameNet.Util

p_sentence :: Element -> Maybe Sentence
p_sentence x = Sentence <$> getOnly1 x "text" ^? _Just.element.contents
                        <*> mapM p_annotationSet (getOnly x "annotationSet")
                        <*> optional (readDecimal =<< x ^. attr "ID")
                        <*> optional (readDecimal =<< x ^. attr "aPOS")
                        <*> optional (readDecimal =<< x ^. attr "paragNo")
                        <*> optional (readDecimal =<< x ^. attr "sentNo")
                        <*> optional (readDecimal =<< x ^. attr "docID")
                        <*> optional (readDecimal =<< x ^. attr "corpID")
                        <*> optional (x ^. attr "externalID")

p_annotationSet :: Element -> Maybe AnnotationSet
p_annotationSet x = AnnotationSet <$> mapM p_layer (getOnly x "layer")
                                  <*> optional (readDecimal =<< x ^. attr "ID")
                                  <*> optional (x ^. attr "status")
                                  <*> optional (x ^. attr "frameName")
                                  <*> optional (readDecimal =<< x ^. attr "frameID")  
                                  <*> optional (x ^. attr "luName")
                                  <*> optional (readDecimal =<< x ^. attr "luID")
                                  <*> optional (x ^. attr "cxnName")
                                  <*> optional (readDecimal =<< x ^. attr "cxnID")
                                  <*> optional (readTime =<< x ^. attr "cDate")

p_layer :: Element -> Maybe Layer
p_layer x = Layer <$> mapM p_label (getOnly x "label")
                  <*> x ^. attr "name"
                  <*> optional (readDecimal =<< x ^. attr "rank")


p_label :: Element -> Maybe Label
p_label x = Label <$> x ^. attr "name"
                  <*> optional (readDecimal =<< x ^. attr "start")
                  <*> optional (readDecimal =<< x ^. attr "end")
                  <*> optional (x ^. attr "fgColor")
                  <*> optional (x ^. attr "bgColor")
                  <*> optional (p_itype =<< x ^. attr "itype")
                  <*> optional (readDecimal =<< x ^. attr "feID")
                  <*> optional (x ^. attr "cBy")

p_itype :: Text -> Maybe IType
p_itype "APos" = Just APos
p_itype "CNI"  = Just CNI
p_itype "INI"  = Just INI
p_itype "DNI"  = Just DNI
p_itype "INC"  = Just INC
p_itype _      = Nothing
