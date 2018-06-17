{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Parser.Common where

import           Control.Lens           ((^.))
import           Text.Taggy.Lens
--
import           FrameNet.Type.Common
import           FrameNet.Util


p_lexeme :: Element -> Maybe Lexeme
p_lexeme x = Lexeme <$> x ^. attr "name"
                    <*> x ^. attr "POS"
                    <*> pure (readBoolean =<< (x ^. attr "breakBefore"))
                    <*> pure (readBoolean =<< (x ^. attr "headword"))
                    <*> pure (readDecimal =<< (x ^. attr "order"))


p_semType :: Element -> Maybe SemType
p_semType x = SemType <$> (readDecimal =<< (x ^. attr "ID"))
                      <*> x ^. attr "name"



