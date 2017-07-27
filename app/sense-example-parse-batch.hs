{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception
import           Control.Lens              hiding (para,(<.>))
import           Control.Monad
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Default
import           Data.Either                      (rights)
import           Data.Foldable
import           Data.Function
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Text.Read                   (decimal)
import           Language.Java                as J
import           System.Directory.Tree
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common (fr_frame)
import           FrameNet.Type.LexUnit
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS
--
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.App.Load
import           OntoNotes.App.Serializer



framesFromLU :: LexUnitDB -> Text -> [Text]
framesFromLU ludb lma = do
  i <- fromMaybe [] (HM.lookup lma (ludb^.nameIDmap))
  l <- maybeToList (IM.lookup i (ludb^.lexunitDB))
  frm <- maybeToList (l^.lexunit_frameReference^.fr_frame)
  return frm


formatExample lma sensemap sensestat = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let examples = filter (not . T.null) . map T.strip . T.lines $ s^.sense_examples
  return (lmav,s^.sense_n,examples)


formatStat :: ((Text,Text),Int) -> String
formatStat ((lma,sens),num) = printf "%20s.%-5s : %5d" lma sens num



mergeStatPB2Lemma ws =
  let merge :: [(Text,Text)] -> (Text,Int)
      merge lst = let (lma,_) = head lst
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))

  in sortBy (flip compare `on` snd) . map merge . groupBy ((==) `on` fst)
     . sortBy (flip compare `on` fst)
     . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
     $ ws

errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()



listSenseExample basedir pp = do
  (_ludb,sensestat,_semlinkmap,sensemap,ws,_wndb) <- loadAll

  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)
  let merged = mergeStatPB2Lemma ws

  withFile (basedir </> "error.log") WriteMode $ \h_err -> 
    forM_ merged $ \(lma,f) -> do
      T.IO.hPutStrLn stdout lma
        
      let lst = formatExample lma sensemap sensestat
      forM_ lst $ \(lmav,sense,txts) -> do
        let bname = T.unpack (lmav <> "_" <> sense)
        withFile (basedir </> bname <.> "corenlp_lemma") WriteMode $ \h_lemma -> do
          withFile (basedir </> bname <.> "corenlp_udep") WriteMode $ \h_ud -> do
            withFile (basedir </> bname <.> "corenlp_ptree") WriteMode $ \h_tr -> do
              errorHandler h_err bname $ do
                anns <- annotateTexts pp txts
                rdocs <- sequenceA <$> traverse protobufDoc anns
                mblma <- serializeLemma rdocs
                mbstr <- serializePennTreeDep rdocs
                case (mblma,mbstr) of
                  (Just bstr_lma, Just (bstr_deps,bstr_ntrs)) -> do
                    BL.hPutStrLn h_lemma bstr_lma
                    BL.hPutStrLn h_ud bstr_deps
                    BL.hPutStrLn h_tr bstr_ntrs
                  _ -> return ()

              


main = do
  args <- getArgs
  let basedir = args !! 0
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                  )
    listSenseExample basedir pp
