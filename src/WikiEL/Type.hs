{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type where

import           Data.Text      (Text)
import qualified Data.Text as T

data EntityToken = EntityToken { word :: Text
                               , tag  :: Text
                               } deriving (Show)

parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> EntityToken (T.dropEnd 1 x) y) $ T.breakOnEnd (T.pack "/") tokenStr

parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)
