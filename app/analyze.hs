{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Lens          hiding (Level)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Foldable
import           Data.Function                    (on)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
-- import qualified Data.IntMap                as IM
import           Data.List                        (intercalate,mapAccumL,zip4)
import           Data.Maybe
import           Data.Map                         (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Time.Calendar               (fromGregorian)
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           Language.Java              as J
import           System.Console.Haskeline
import           System.FilePath
import           System.Environment
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.ProtocolBuffers.Basic       (Utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           FrameNet.Query.Frame
import           FrameNet.Type.Common
import           FrameNet.Type.Frame             hiding (LexUnit)
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import           PropBank.Util
import           Text.Annotation.Type
import           Text.Annotation.Util.Doc
import           Text.Annotation.View


import           WikiEL                                       (loadEMtagger)
import           WikiEL.CoreNLP                               (parseNEROutputStr)
import           WikiEL.WikiEntityTagger                      (loadWETagger,wikiAnnotator)
import           WikiEL.WikiEntityClass                       (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator,resolvedUID)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..),entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.ETL.LoadData
-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity          as N
import qualified WikiEL.WikiEntityClass        as WC
import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.WordNet
import qualified WikiEL.EntityLinking          as EL
import qualified WikiEL.Type.FileFormat        as F
import qualified WikiEL                        as WEL


--
import           OntoNotes.App.Load
import           OntoNotes.App.Util
import           OntoNotes.Corpus.Load
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.Type.SenseInventory

{-
convertToken_charIndex :: TK.Token -> Maybe Token
convertToken_charIndex t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)
-}

formatLemmaPOS :: Token -> String
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))



getSentenceOffsets :: [S.Sentence] -> [(SentIdx,BeginEnd)]
getSentenceOffsets psents =
  zip ([1..] :: [Int]) $ flip map psents $ \s ->
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (fromIntegral b+1,fromIntegral e)



addSUTime :: [SentItem] -> T.ListTimex
          -> [(SentItem,[TagPos (Maybe Utf8)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
  in filter (not.null.(^._2)) $ map (addTag (map f (tmxs^..T.timexes.traverse))) sents



formatTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> IO ()
formatTimex (s,a) = do
  T.IO.putStrLn (T.intercalate "\n" (underlineText (const "") (s^._2) (s^._3) a))
  T.IO.putStrLn "----------"
  print a


runParser :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> Text
          -> IO ([S.Sentence], [Maybe Sentence], [[Token]], [Maybe PennTree], [Dependency], Maybe [(SentItem, [TagPos (Maybe Utf8)])])
runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> do
      let psents = toListOf (D.sentence . traverse) pdoc
          sentidxs = getSentenceOffsets psents
          sents = map (addText txt) sentidxs
          sentswithtmx = addSUTime sents rsutime
      -- mapM_ formatResult sentswithtmx
      return (Just sentswithtmx)
  let psents = getProtoSents pdoc
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  return (psents,sents,tokss,parsetrees,deps,mtmx)


getSenses :: Text -> HashMap Text Inventory -> HashMap (Text,Text) Int -> FrameDB -> HashMap Text [(Text,Text)]
          -> [(Text,Text,Int,Text,Text,Text,Text)]
getSenses lma sensemap sensestat framedb ontomap = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = T.take 40 (s^.sense_name)
      (txt_frame,txt_fecore,txt_feperi)
        = fromMaybe ("","","") $ do
            lst <- HM.lookup lma ontomap
            frtxt <- lookup (s^.sense_group <> "." <> s^.sense_n) lst
            case frtxt of
              "copula"    -> return ("** COPULA **"    , "","")
              "idioms"    -> return ("** IDIOMS **"    , "","")
              "lightverb" -> return ("** LIGHT VERB **", "","")
              _ -> do
                frame <- HM.lookup frtxt (framedb^.frameDB)
                let fes = frame^..frame_FE.traverse
                    corefes = filter (\fe -> fe^.fe_coreType == Core) fes
                    perifes = filter (\fe -> fe^.fe_coreType == Peripheral) fes
                    fecoretxt = T.intercalate ", " (map (^.fe_name) corefes)
                    feperitxt = T.intercalate ", " (map (^.fe_name) perifes)
                return (frtxt,fecoretxt,feperitxt)
  return (s^.sense_group,s^.sense_n,num,txt_def,txt_frame,txt_fecore,txt_feperi)


chooseFrame :: [(Text,Text,Int,Text,Text,Text,Text)] -> Maybe (Text,Text,Int,Text,Text,Text,Text)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._3)) xs)


formatSense :: (Text,Text,Int,Text,Text,Text,Text) -> String
formatSense (sgrp,sn,num,txt_def,txt_frame,txt_fecore,txt_feperi) = 
  printf "%2s.%-6s (%4d cases) | %-40s | %-20s | %-40s      ------       %-30s " sgrp sn num txt_def txt_frame txt_fecore txt_feperi


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [(Text,Text,Int,Text,Text,Text,Text)]
             -> String
formatSenses doesShowOtherSense lst
  = let t = chooseFrame lst
    in "Top frame: "
       ++ printf " %-20s | %-40s      ------      %-30s\n"
            (fromMaybe "" (t^?_Just._5))
            (fromMaybe "" (t^?_Just._6))
            (fromMaybe "" (t^?_Just._7))
       ++ if doesShowOtherSense
          then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense lst)
          else ""


sentStructure :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
              -> HashMap Text Inventory
              -> HashMap (Text,Text) Int
              -> FrameDB
              -> HashMap Text [(Text,Text)]
              -> Text
              -> IO ()
sentStructure pp sensemap sensestat framedb ontomap txt = do
  (psents,sents,_tokss,mptrs,deps,mtmx) <- runParser pp txt
  putStrLn "\n\n\n\n\n\n\n\n================================================================================================="
  case mtmx of
    Nothing -> putStrLn "Time annotation not successful!"
    Just sentswithtmx -> mapM_ formatTimex sentswithtmx
  putStrLn "-------------------------------------------------------------------------------------------------"

  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,_sent,mptr,_dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let lemmamap = mkLemmaMap psent
          vps = verbPropertyFromPennTree lemmamap ptr

      
      putStrLn "--------------------------------------------------------------------------------------------------"
      T.IO.putStrLn (formatIndexTokensFromTree 0 ptr)
      
      putStrLn "--------------------------------------------------------------------------------------------------"
      showClauseStructure lemmamap ptr
      putStrLn "================================================================================================="

      forM_ (vps^..traverse.vp_lemma.to unLemma) $ \lma -> do
        putStrLn (printf "Verb: %-20s" lma)
        let senses = getSenses lma sensemap sensestat framedb ontomap
        (putStrLn . formatSenses False) senses
        putStrLn "--------------------------------------------------------------------------------------------------"


-- -- abandoned code to simplify the resultant text.
      -- let tkns = zip [0..] (getTKTokens psent)
      -- tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
      -- itr = mkAnnotatable (mkPennTreeIdx ptr)
      -- iltr = lemmatize lmap itr
      -- idltr = depLevelTree dep iltr
      -- vtree = verbTree vps idltr
      -- let lmaposs = concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
      --     lmas = map (^.token_lemma) lmaposs



queryProcess :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> HashMap Text Inventory
             -> HashMap (Text,Text) Int
             -> FrameDB
             -> HashMap Text [(Text,Text)]
             -> IO ()
queryProcess pp sensemap sensestat framedb ontomap =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
    sentStructure pp sensemap sensestat framedb ontomap input
    putStrLn "=================================================================================================\n\n\n\n"


main0 :: IO ()
main0 = do
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                  )
    queryProcess pp sensemap sensestat framedb ontomap



--
--
-- wiki-ner test
--
--





groupupheredir = "/scratch/groups/uphere"
wikinerdir = groupupheredir </> "wiki-ner"

listedCompanyFile = groupupheredir </> "enwiki/companies"
newsFileTxt = wikinerdir </> "data/article.amazon_nike.txt"
-- rawNewsFile3 = wikinerdir </> "data/article.amazon_nike.ptb"
-- nerNewsFile3 = wikinerdir </> "data/article.amazon_nike.ner"
reprFile     = EntityReprFile (wikinerdir </> "data/uid")
orgItemFile  = ItemIDFile (wikinerdir </> "data/ne.org")
personItemFile = ItemIDFile (wikinerdir </> "data/ne.person")
brandItemFile  = ItemIDFile (wikinerdir </> "data/ne.brand")
wordnetMappingFile = WordNetMappingFile (wikinerdir </> "data/page_id.wiki_id.wordnet.tsv")



getOrgs :: EntityMention a -> Maybe (EntityMentionUID, ItemID)
getOrgs (EL.Self muid (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs (EL.Cite muid _ (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs _ = Nothing


getCompanySymbol :: Map ItemID Symbol -> (EntityMentionUID, ItemID) -> Maybe (EntityMentionUID , ItemID, Symbol)
getCompanySymbol tikcerMap (mentionUID, itemID) = result
  where
    result = case M.lookup itemID tikcerMap of
      Just symbol -> Just (mentionUID, itemID, symbol)
      Nothing     -> Nothing  

linkedMentionToTagPOS :: [Token]
                      -> UIDCite EntityMentionUID (EL.EMInfo Text)
                      -> Maybe (TagPos EntityMentionUID)
linkedMentionToTagPOS toks linked_mention = do
  let uid = EL._uid linked_mention
      IRange b e = (_info linked_mention)^._1
      matched_toks = filter (\tok -> (tok^.token_tok_idx_range) `isInsideR` (b,e)) toks
  guard ((not.null) matched_toks)
  let cb = (head matched_toks)^.token_char_idx_range._1
      ce = (last matched_toks)^.token_char_idx_range._2
      tagpos = (cb+1,ce,uid)
  return tagpos


formatTaggedSentences sents_tagged =
  let txts = concatMap (\(s,a) -> underlineText (T.pack . show . EL._emuid) (s^._2) (s^._3) a) sents_tagged
  in vcat top $ map (text . T.unpack) txts 

formatPreNE tag = case resolvedUID tag of
                    Left e -> "unresolved"
                    Right i -> show i


formatEMInfo :: EL.EMInfo Text -> String
formatEMInfo em@(_,ws,tag) = printf "%-25s %-20s" (WEL.entityName em) (formatPreNE tag)


formatLinkedMention Cite {..} = printf "%3d: (-> %3d) %s " (EL._emuid _uid) (EL._emuid _ref) (formatEMInfo _info)
formatLinkedMention Self {..} = printf "%3d:          %s " (EL._emuid _uid)                  (formatEMInfo _info)


main :: IO ()
main = do
  file <- T.IO.readFile listedCompanyFile
  txt <- T.IO.readFile newsFileTxt
  emTagger <- loadEMtagger reprFile [(WC.orgClass, orgItemFile), (WC.personClass, personItemFile), (WC.brandClass, brandItemFile)]

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       -- . (sutime .~ True)
                       -- . (constituency .~ True)
                       . (ner .~ True)
                  )
    -- queryProcess pp sensemap sensestat framedb ontomap
    let doc = Document txt (fromGregorian 2017 4 17)
    ann <- annotate pp doc
    rdoc <- protobufDoc ann
    case rdoc of
      Left _ -> return ()
      Right d -> do
        let psents = d ^.. D.sentence . traverse
            sentidxs = getSentenceOffsets psents
            sents = map (addText txt) sentidxs 
            unNER (NERSentence tokens) = tokens
            neTokens = concatMap (unNER . sentToNER) psents
            linked_mentions = emTagger neTokens
            toks = concatMap (map snd . sentToTokens) psents
            tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions
            sents_tagged = map (addTag tags) sents
            doc1 = formatTaggedSentences sents_tagged
            doc2 = vcat top $ map (text.formatLinkedMention) linked_mentions
            doc = hsep 10 left [doc1,doc2]
        putStrLn (render doc)



