{-# LANGUAGE OverloadedStrings #-}

module HFrameNet.Util where

import           Control.Lens           ((^?),(^.),(^..),_Just,only )
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text        as T
import           Data.Text.Read
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (parseTimeM, defaultTimeLocale)
import           Text.Taggy.Lens


readDecimal :: Text -> Maybe Int
readDecimal = either (\_ -> Nothing) (Just . fst) . decimal 


readBoolean :: Text -> Maybe Bool
readBoolean "true" = Just True
readBoolean "false" = Just False
readBoolean _ = Nothing

readTime :: Text -> Maybe UTCTime
readTime txt = parseTimeM True defaultTimeLocale "%m/%d/%0Y %H:%M:%S %Z %a" (T.unpack txt)



getOnly x k = listToMaybe (x ^.. elements . named (only k))
