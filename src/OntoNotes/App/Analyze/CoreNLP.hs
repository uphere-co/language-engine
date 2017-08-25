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
import           CoreNLP.Simple.Convert                       (convertPsent,convertSentence,convertToken
                                                              ,decodeToPennTree
                                                              ,sentToDep,sentToNER)
import           CoreNLP.Simple.Util                          (getDoc,getProtoDoc,getTKTokens)
import 	       	 NLP.Type.CoreNLP                             (NERSentence(..),Token,Dependency,Sentence,SentenceIndex)
import qualified NLP.Type.NamedEntity                  as N
import           NLP.Type.PennTreebankII                      (PennTree)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..)
                                                              ,entityLinking,entityLinkings,buildEntityMentions,entityUID)
import qualified WikiEL.EntityLinking                  as EL
--
import           OntoNotes.App.Util                           (BeginEnd,TagPos,SentItem,SentIdx
                                                              ,CharIdx,TokIdx
                                                              ,addSUTime,addTag,addText
                                                              ,listTimexToTagPos
                                                              ,underlineText)
import           OntoNotes.App.WikiEL                         (getWikiResolvedMentions)


getSentenceOffsets :: [S.Sentence] -> [(SentIdx,BeginEnd CharIdx)]
getSentenceOffsets psents =
  zip ([1..] :: [Int]) $ flip map psents $ \s ->
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (fromIntegral b+1,fromIntegral e)


runParser :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO ( [Sentence]
                , [Maybe SentenceIndex] -- [Maybe Sentence]
                , [SentItem CharIdx]
                , [[Token]]
                , [Maybe PennTree]
                , [Dependency]
                -- , Maybe [(SentItem CharIdx, [TagPos CharIdx (Maybe Text)])]
                , Maybe [TagPos TokIdx (Maybe Text)]
                )
runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentidxs = getSentenceOffsets psents
      sentitems = map (addText txt) sentidxs


  let parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sentidxs = map (convertSentence pdoc) psents
      sents = map (convertPsent) psents
      Right deps = mapM sentToDep psents
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
      toks = concat tokss

  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> return (Just (listTimexToTagPos rsutime))
      -- let sentswithtmx = addSUTime sentitems toks rsutime
      -- return (Just sentswithtmx)

  return (sents,sentidxs,sentitems,tokss,parsetrees,deps,mtmx)


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
