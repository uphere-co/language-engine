module OntoNotes.App.Serializer where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Data.IntMap                  as IM
import           Data.Maybe
import qualified Data.Sequence                as Seq
import           Data.Time.Calendar
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           NLP.Type.PennTreebankII



annotateTexts pp txts = let docs = map (flip Document (fromGregorian 1990 1 1)) txts
                        in traverse (annotate pp) docs

serializeLemma pp anns {- h_lemma -} = do
  rdocs' <- traverse protobufDoc anns
  let rdocs = sequenceA rdocs'
  case rdocs of
    Left err -> print err >> return Nothing
    Right ds -> do
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          lmap= map (map (_2 %~ unLemma) . IM.toList . mkLemmaMap) sents
      -- BL.hPutStrLn h_lemma (encode lmap)
      return (Just (encode lmap))


serializePennTreeDep pp anns {- (h_ud,h_tr) -} = do
  rdocs' <- traverse protobufDoc anns
  let rdocs = sequenceA rdocs'
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
          {- BL.hPutStrLn h_ud (encode deps)
          BL.hPutStrLn h_tr (encode ntrs) -}

