{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Util where

import           Data.Text              (Text)
import qualified Data.Text        as T
import           Data.Text.Read
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (parseTimeM, defaultTimeLocale)


readDecimal :: Text -> Maybe Int
readDecimal = either (\_ -> Nothing) (Just . fst) . decimal 


readBoolean :: Text -> Maybe Bool
readBoolean "true" = Just True
readBoolean "false" = Just False
readBoolean _ = Nothing

readTime :: Text -> Maybe UTCTime
readTime txt = parseTimeM True defaultTimeLocale "%m/%d/%0Y %H:%M:%S %Z %a" (T.unpack txt)


