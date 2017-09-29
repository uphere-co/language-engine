{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WikiEL.CoreNLP where

import           Data.Text                         (Text)
import qualified Data.Text                  as T

newtype WordToken = WordToken   { unWord :: Text}
                  deriving (Show, Eq, Ord)
newtype NETag     = NETag { unNETag :: Text}
                  deriving (Show, Eq, Ord)

data EntityToken = EntityToken { word :: WordToken
                               , tag  :: NETag }
                 deriving (Show)

{-|
  This module is for parsing an output format of CoreNLP NER in CLI mode.
-}
parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> (EntityToken (WordToken (T.dropEnd 1 x)) (NETag y))) $ T.breakOnEnd (T.pack "/") tokenStr

parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)
