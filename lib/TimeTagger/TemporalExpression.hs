{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeTagger.TemporalExpression where

import           Control.Lens
import           Control.Monad                    (forM)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Int                         (Int32)
import           Data.List                        (findIndices)
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import           Data.Time.Calendar               (fromGregorian)
import           Language.Java              as J
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Timex as T
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           Text.TaggedText
