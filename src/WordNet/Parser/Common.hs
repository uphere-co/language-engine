{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Common where

import           Data.Text           (Text)
import qualified Data.Text      as T
import           Data.Text.Read      (decimal)

isComment :: Text -> Bool
isComment t | T.length t > 2 = T.take 2 t == "  "
            | otherwise    = True

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

readDecimal :: Text -> Maybe Int
readDecimal = fmap fst . (eitherToMaybe . decimal)
