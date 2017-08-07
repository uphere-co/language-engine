{-# LANGUAGE DataKinds #-}

module OntoNotes.App.Serializer where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Data.IntMap                  as IM
import           Data.Maybe
import qualified Data.Sequence                as Seq
import           Data.Text                           (Text)
import           Data.Time.Calendar
import           Language.Java
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           NLP.Type.PennTreebankII


annotateTexts :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
              -> [Text]
              -> IO [J ('Class "edu.stanford.nlp.pipeline.Annotation")]
annotateTexts pp txts = let docs = map (flip Document (fromGregorian 1990 1 1)) txts
                        in traverse (annotate pp) docs

serializeLemma :: Either String [D.Document] -> IO (Maybe BL.ByteString)
serializeLemma rdocs = do
  case rdocs of
    Left err -> print err >> return Nothing
    Right ds -> do
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          lemmamap= map (map (_2 %~ unLemma) . IM.toList . mkLemmaMap) sents
      return (Just (encode lemmamap))


serializePennTreeDep :: Either String [D.Document] -> IO (Maybe (BL.ByteString,BL.ByteString))
serializePennTreeDep rdocs = do
  case rdocs of
    Left err -> print err >> return Nothing
    Right ds -> do
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          ntrs = map decodeToPennTree (mapMaybe (^.S.parseTree) sents)
          edeps = mapM sentToDep sents
      case edeps of
        Left err -> print err >> return Nothing
        Right deps -> do
          return (Just (encode deps,encode ntrs))


