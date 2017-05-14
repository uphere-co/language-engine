{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Common where

import Data.Text (Text)
import qualified Data.Text as T

isComment :: Text -> Bool
isComment t | T.length t > 2 = T.take 2 t == "  "
            | otherwise    = True

eitherToMaybe (Left x) = Nothing
eitherToMaybe (Right x) = Just x
