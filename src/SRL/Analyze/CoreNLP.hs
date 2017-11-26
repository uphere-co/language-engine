{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.CoreNLP where

import           Control.Applicative                          ((<|>))
import           Control.Lens                                 ((^.),(^..),to,toListOf)
import           Control.Monad                                (guard)
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Foldable                                (toList,traverse_)
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
                                                              ,decodeToPennTree,sentToDep,mkLemmaMap)
import           CoreNLP.Simple.Util                          (getDoc,getProtoDoc,getTKTokens)
import           HUKB.PPR                                     (ppr)
import           HUKB.Type                                    (Context(..),ContextWord(..))
import           WordNet.Type.POS                             (POS(..))
import           NLP.Type.CoreNLP                             (Sentence,sentenceLemma)
--
import           SRL.Analyze.Type                             (DocAnalysisInput(..))
import           SRL.Analyze.Util                             (addText,listTimexToTagPos,getSentenceOffsets)

import qualified Data.Text.IO as TIO
import Data.Attribute (ahead)
import NLP.Syntax.Util (mkBitreeICP)
import           NLP.Type.PennTreebankII                      (Lemma(..),tokenWord,posTag,getAnnot,isNoun,isVerb,TernaryLogic(..))
import           NLP.Printer.PennTreebankII                   (prettyPrint)
-- import NLP.Syntax.

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


posTagToPOS p = (guard (isVerb p)        >> return POS_V) <|>
                (guard (isNoun p == Yes) >> return POS_N) <|>
                Nothing


runUKB sents parsetrees = do
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
  flip mapM_ (zip lmass parsetrees) $ \(lmas,mpt) ->
    flip traverse_ mpt $ \pt -> do
      let lmap = (mkLemmaMap . map unLemma) lmas
          lemmapt = mkBitreeICP lmap pt
          mkContextWord (i,x) = CtxtWord (unLemma (ahead (getAnnot x))) <$> posTagToPOS (posTag x) <*> pure i <*> pure 1
          ctxt = Context "0" (mapMaybe mkContextWord (toList lemmapt))
      print ctxt
      result <- ppr ctxt
      putStrLn ("\n\nTHIS RESULT: \n" ++ show result)
  


runParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO DocAnalysisInput
runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann

  let psents = pdoc ^.. D.sentence . traverse
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
