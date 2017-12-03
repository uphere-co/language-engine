{-# LANGUAGE DeriveGeneric #-}

module Type where

import Data.Aeson
import Data.Text     (Text)
import GHC.Generics
--
import NLP.Type.TagPos
import WikiEL.Type
--
import SRL.Analyze.Type


data Test = Test { _test_id :: Text
                 , _test_text :: Text
                 , _test_dainput :: DocAnalysisInput
                 , _test_ner :: [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
                 }
          deriving (Show,Generic)

instance FromJSON Test

instance ToJSON Test

