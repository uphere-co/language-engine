{-# LANGUAGE DataKinds #-}

module SRL.Analyze.CoreNLP where

import           Control.Lens                                 ((^.),toListOf)
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Maybe                                   (mapMaybe)
import           Data.Text                                    (Text)
import qualified Language.Java                         as J
import           Text.ProtocolBuffers.WireMessage             (messageGet)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import           CoreNLP.Simple                               (annotate,serializeTimex)
import           CoreNLP.Simple.Convert                       (convertPsent,convertSentence,convertToken
                                                              ,decodeToPennTree,sentToDep)
import           CoreNLP.Simple.Util                          (getDoc,getProtoDoc,getTKTokens)
import           NLP.Type.CoreNLP                             (Sentence)
--
import           SRL.Analyze.Type                             (DocAnalysisInput(..))
import           SRL.Analyze.Util                             (addText,listTimexToTagPos,getSentenceOffsets)


runParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO DocAnalysisInput
runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentitems = map (addText txt) (getSentenceOffsets psents)
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sentidxs = map (convertSentence pdoc) psents
      sents = map (convertPsent) psents
      Right deps = mapM sentToDep psents
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> return (Just (listTimexToTagPos rsutime))
  return (DocAnalysisInput sents sentidxs sentitems tokss parsetrees deps mtmx)


preRunParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO ( [Sentence] )
preRunParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sents = map (convertPsent) psents
  return sents
