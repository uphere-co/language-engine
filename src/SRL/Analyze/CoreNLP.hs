{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           HUKB.PPR                                     (ppr)
import           HUKB.Type                                    (Context(..),ContextWord(..))
import           WordNet.Type.POS                             (POS(..))
import           NLP.Type.CoreNLP                             (Sentence)
--
import           SRL.Analyze.Type                             (DocAnalysisInput(..))
import           SRL.Analyze.Util                             (addText,listTimexToTagPos,getSentenceOffsets)


emptyDocAnalysisInput :: DocAnalysisInput
emptyDocAnalysisInput = DocAnalysisInput
  { _dainput_sents     = []
  , _dainput_sentidxs  = []
  , _dainput_sentitems = []
  , _dainput_tokss     = [[]]
  , _dainput_mptrs     = []
  , _dainput_deps      = []
  , _dainput_mtmxs     = Nothing
  }

runParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO DocAnalysisInput
runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann


  -- testing HUKB
  let cwords = zipWith f [1..] [ ("Britain",POS_N), ("cut",POS_V), ("stamp",POS_N), ("duty",POS_N), ("property",POS_N), ("tax",POS_N)
                               , ("buyer",POS_N), ("bid",POS_N), ("help",POS_V), ("people",POS_N), ("struggle",POS_V), ("get",POS_V)
                               , ("property",POS_N), ("ladder",POS_N), ("finance",POS_N), ("minister",POS_N), ("Philip",POS_N)
                               , ("Hammond",POS_N), ("say",POS_V), ("Wednesday",POS_N) ]
        where f i (w,p) = CtxtWord w p i 1
  let ctxt = Context "1" cwords
  result <- ppr ctxt
  print result
  --- end testing



  let psents = toListOf (D.sentence . traverse) pdoc
      sentitems = map (addText txt) (getSentenceOffsets psents)
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sentidxs = map (convertSentence pdoc) psents
      sents = map (convertPsent) psents
      edeps = mapM sentToDep psents
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> return (Just (listTimexToTagPos rsutime))
  case edeps of
    Left  _    -> return emptyDocAnalysisInput
    Right deps -> return (DocAnalysisInput sents sentidxs sentitems tokss parsetrees deps mtmx)


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
