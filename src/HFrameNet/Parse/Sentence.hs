{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Parse.Sentence where

import           Control.Applicative
import           Control.Lens ((^?),(^.),(^..),_Just,only)
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Common
import           HFrameNet.Type.Sentence
import           HFrameNet.Util

p_sentence :: Element -> Maybe Sentence
p_sentence x = Sentence <$> getOnly1 x "text" ^? _Just.element.contents
                        <*> mapM p_annotationSet (getOnly x "annotationSet")

p_annotationSet :: Element -> Maybe AnnotationSet
p_annotationSet x = AnnotationSet <$> mapM p_layer (getOnly x "layer")

p_layer :: Element -> Maybe Layer
p_layer x = Layer <$> mapM p_label (getOnly x "label")
                  <*> x ^. attr "name"
                  <*> pure (readDecimal =<< x ^. attr "rank")


p_label :: Element -> Maybe Label
p_label x = Label <$> x ^. attr "name"
                  <*> pure (readDecimal =<< x ^. attr "start")
                  <*> pure (readDecimal =<< x ^. attr "end")
                  <*> pure (x ^. attr "fgColor")
                  <*> pure (x ^. attr "bgColor")
