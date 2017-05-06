{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Parse.Common where

import           Control.Lens           ((^?),(^.),(^..),_Just,only)
import           Text.Taggy.Lens
--
import           HFrameNet.Type.Common
import           HFrameNet.Util


p_lexeme :: Element -> Maybe Lexeme
p_lexeme x = Lexeme <$> x ^. attr "name"
                    <*> x ^. attr "POS"
                    <*> pure (readBoolean =<< (x ^. attr "breakBefore"))
                    <*> pure (readBoolean =<< (x ^. attr "headword"))
                    <*> pure (readDecimal =<< (x ^. attr "order"))


