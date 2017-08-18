{-# LANGUAGE DataKinds #-}

module OntoNotes.App.Analyze.CoreNLP where

import           Control.Lens                                 ((^.),(^..),_2,firstOf,lastOf,toListOf)
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Maybe                                   (fromJust,mapMaybe)
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import qualified Language.Java                         as J
import           Text.ProtocolBuffers.Basic                   (Utf8)
import           Text.ProtocolBuffers.WireMessage             (messageGet)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple                               (annotate,serializeTimex)
import           CoreNLP.Simple.Convert                       (convertSentence,convertToken,decodeToPennTree
                                                              ,sentToDep,sentToNER)
import           CoreNLP.Simple.Type.Simplified               (NERSentence(..),Token,Dependency,SentenceIndex)
import           CoreNLP.Simple.Util                          (getDoc,getProtoDoc,getTKTokens)
import qualified NLP.Type.NamedEntity                  as N
import           NLP.Type.PennTreebankII                      (PennTree)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..)
                                                              ,entityLinking,entityLinkings,buildEntityMentions,entityUID)
import qualified WikiEL.EntityLinking                  as EL
--
import           OntoNotes.App.Util                           (BeginEnd,TagPos,SentItem,SentIdx,addTag,addText,underlineText)



addSUTime :: [SentItem] -> T.ListTimex
          -> [(SentItem,[TagPos (Maybe Utf8)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
  in filter (not.null.(^._2)) $ map (addTag (map f (tmxs^..T.timexes.traverse))) sents


getSentenceOffsets :: [S.Sentence] -> [(SentIdx,BeginEnd)]
getSentenceOffsets psents =
  zip ([1..] :: [Int]) $ flip map psents $ \s ->
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (fromIntegral b+1,fromIntegral e)


runParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
          -> Text
          -> IO ( [S.Sentence]
                , [Maybe SentenceIndex] -- [Maybe Sentence]
                , [(SentIdx,BeginEnd,Text)]                  
                , [[Token]]
                , [Maybe PennTree]
                , [Dependency]
                , Maybe [(SentItem, [TagPos (Maybe Utf8)])]
                , [UIDCite EntityMentionUID (EL.EMInfo Text)]
                )
runParser pp emTagger txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentidxs = getSentenceOffsets psents
      sentitems = map (addText txt) sentidxs
  
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> do
      let sentswithtmx = addSUTime sentitems rsutime
      return (Just sentswithtmx)
  let parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss

      unNER (NERSentence tokens) = tokens
      neTokens = concatMap (unNER . sentToNER) psents
      linked_mentions_all = emTagger neTokens
      linked_mentions_resolved
        = filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all
  return (psents,sents,sentitems,tokss,parsetrees,deps,mtmx,linked_mentions_resolved)
