{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.IntMap                as IM
import           Data.List                        (intercalate,foldl',sort,zip4)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO

import           Language.Java              as J
import           System.Console.Haskeline
import           System.Directory.Tree
import           System.Environment
import           System.FilePath
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
import           FrameNet.Type.Frame hiding (LexUnit)
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import           SRL.Feature
import           SRL.Feature.Dependency
import           Type
import           Util.Doc
import           View
--
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.App.Load


convertToken_charIndex :: TK.Token -> Maybe Token
convertToken_charIndex t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)

formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


type SentIdx = Int
type CharIdx = Int
type BeginEnd = (CharIdx,CharIdx)
type TagPos a = (CharIdx,CharIdx,a)
type SentItem = (SentIdx,BeginEnd,Text)


getSentenceOffsets :: D.Document -> [(SentIdx,BeginEnd)]
getSentenceOffsets doc =
  let sents = toListOf (D.sentence . traverse) doc
  in zip ([1..] :: [Int]) $ flip map sents $ \s ->
       let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
           e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
       in (fromIntegral b+1,fromIntegral e)


addText :: Text -> (SentIdx,BeginEnd) -> SentItem
addText txt (n,(b,e)) = (n,(b,e),slice (b-1) e txt)


addTag :: [TagPos a] -> SentItem -> (SentItem,[TagPos a])
addTag lst i@(_,(b,e),_) = (i,filter check lst)
  where check (b',e',_) = b' >= b && e' <= e


addSUTime :: [SentItem] -> T.ListTimex
          -> [(SentItem,[TagPos (Maybe Utf8)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
  in filter (not.null.(^._2)) $ map (addTag (map f (tmxs^..T.timexes.traverse))) sents



underlineText :: BeginEnd -> Text -> [TagPos a] -> IO ()
underlineText (b0,_e0) txt lst = do
  let f (b,e,_) = ((),b-b0+1,e-b0+1)
      tagged = map f lst
      ann = (AnnotText . map (\(t,m)-> (t,isJust m)) . tagText tagged) txt
      xss = lineSplitAnnot 80 ann
  sequence_ (concatMap (map cutePrintAnnot) xss)


formatTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> IO ()
formatTimex (s,a) = do
  -- T.IO.putStrLn $ "Sentence " <> T.pack (show (s^._1))
  underlineText (s^._2) (s^._3) a
  T.IO.putStrLn "----------"
  print a


runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left err -> return Nothing
    Right rsutime -> do
      let sentidxs = getSentenceOffsets pdoc
          sents = map (addText txt) sentidxs
          sentswithtmx = addSUTime sents rsutime
      -- mapM_ formatResult sentswithtmx
      return (Just sentswithtmx)
  let psents = getProtoSents pdoc
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken_charIndex) tktokss
  return (psents,sents,tokss,parsetrees,deps,mtmx)



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

chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._3)) xs)


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

sentStructure pp sensemap sensestat framedb ontomap txt = do
  (psents,sents,tokss,mptrs,deps,mtmx) <- runParser pp txt
  putStrLn "\n\n\n\n\n\n\n\n================================================================================================="
  -- T.IO.putStrLn txt
  case mtmx of
    Nothing -> putStrLn "Time annotation not successful!"
    Just sentswithtmx -> mapM_ formatTimex sentswithtmx
  putStrLn "-------------------------------------------------------------------------------------------------"

  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,sent,mptr,dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let tkns = zip [0..] (getTKTokens psent)
          tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)

      let itr = mkAnnotatable (mkPennTreeIdx ptr)
          lmap= mkLemmaMap psent
          iltr = lemmatize lmap itr
          idltr = depLevelTree dep iltr
          vps = verbPropertyFromPennTree lmap ptr
          vtree = verbTree vps idltr
      putStrLn "--------------------------------------------------------------------------------------------------"
      T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip [0..] . map snd . toList $ ptr

      putStrLn "--------------------------------------------------------------------------------------------------"
      let lmaposs = concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
          lmas = map (^.token_lemma) lmaposs
      showClauseStructure lmap ptr
      putStrLn "================================================================================================="

      forM_ (vps^..traverse.vp_lemma.to unLemma) $ \lma -> do
        putStrLn (printf "Verb: %-20s" lma)
        let senses = getSenses lma sensemap sensestat framedb ontomap
        (putStrLn . formatSenses False) senses
        putStrLn "--------------------------------------------------------------------------------------------------"







queryProcess pp sensemap sensestat framedb ontomap = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
    sentStructure pp sensemap sensestat framedb ontomap input
    putStrLn "=================================================================================================\n\n\n\n"

main = do
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)

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
