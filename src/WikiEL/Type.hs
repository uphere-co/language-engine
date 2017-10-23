{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type where

import           Data.Aeson
import           Data.Text      (Text)
import qualified Data.Text as T
import           GHC.Generics   (Generic)

data EntityToken = EntityToken { word :: Text
                               , tag  :: Text
                               } deriving (Show)

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq,Generic)

instance ToJSON IRange where
  toJSON = genericToJSON defaultOptions

instance FromJSON IRange where
  parseJSON = genericParseJSON defaultOptions

instance Show IRange where
  show (IRange beg end) = "IRange [" ++ show beg ++ "," ++ show end ++ ")"

data RelativePosition = LbeforeR | RbeforeL | Coincide | RinL | LinR | LoverlapR | RoverlapL
                      deriving(Show,Eq)

parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> EntityToken (T.dropEnd 1 x) y) $ T.breakOnEnd (T.pack "/") tokenStr

parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)
