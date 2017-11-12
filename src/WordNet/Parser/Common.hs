{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Common where

import           Data.Text           (Text)
import qualified Data.Text      as T
import           Data.Text.Read      (decimal,hexadecimal)
--
import           WordNet.Type.POS


isComment :: Text -> Bool
isComment t | T.length t > 2 = T.take 2 t == "  "
            | otherwise    = True

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

readDecimal :: Text -> Maybe Int
readDecimal = fmap fst . (eitherToMaybe . decimal)

readHexadecimal :: Text -> Maybe Int
readHexadecimal = fmap fst . (eitherToMaybe . hexadecimal)


readPOS :: Char -> Maybe POS
readPOS 'n' = Just POS_N
readPOS 'v' = Just POS_V
readPOS 'a' = Just POS_A
readPOS 'r' = Just POS_R
readPOS _   = Nothing



readSSType :: Char -> Maybe SSType
readSSType 'n' = Just Noun
readSSType 'v' = Just Verb
readSSType 'a' = Just Adjective
readSSType 's' = Just AdjectiveSatellite
readSSType 'r' = Just Adverb
readSSType _   = Nothing
