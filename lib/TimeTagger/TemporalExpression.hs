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

mkRangeVal st = zip3 (st ^.. T.timexes . traverse . T.characterOffsetBegin) (st ^.. T.timexes . traverse . T.characterOffsetEnd) (st ^.. T.timexes . traverse . T.timex . T.value) 

-- findTemporalTokens :: [(Int32, Int32, Maybe Text.ProtocolBuffers.Basic.Utf8)] -> [[Token]] -> IO [(t3, [Int])]
findTemporalTokens rv tks = do
  result <- forM rv $ \(i,f,v) -> return $ (v,findIndices (\x -> (fromIntegral i <= x ^. token_range ^. _1) && (x ^. token_range ^. _2 <= fromIntegral f)) (concat tks))
  return result

getTemporalExp :: Text -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> IO [TagInfo (Maybe Text)]
getTemporalExp txt pp = do
  let doc = Document txt (fromGregorian 2017 4 17)
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertTokenInCharOffset) tktokss
  (esuetime_result :: Either String T.ListTimex) <- (((fmap fst) . messageGet . BL.fromStrict) <$> serializeTimex ann)
  case esuetime_result of
    Left  _              -> return []
    Right suetime_result -> do
      let range = mkRangeVal suetime_result
      result <- findTemporalTokens range tokss
      return $ map (\(v,xs) -> TagInfo {_taginfo_range = (head xs,last xs), _taginfo_metainfo = Just $ MetaInfo { _metainfo_info = fmap cutf8 v}, _taginfo_text = T.intercalate " " $ map (\x -> ((concat tokss) !! x) ^. token_text) xs}) result
