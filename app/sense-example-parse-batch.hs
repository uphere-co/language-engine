{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
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
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
--
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




listSenseExample pp = do
  (_ludb,sensestat,_semlinkmap,sensemap,ws,_wndb) <- loadAll

  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)
  let merged = mergeStatPB2Lemma ws

  forM_ (take 10 merged) $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma
    let lst = formatExample lma sensemap sensestat
    forM_ lst $ \(lmav,sense,txts) -> do

      
    {- 
    let doc = text "=====================================================================================================================" //
              text (printf "%20s:%6d " lma f) //
              text "---------------------------------------------------------------------------------------------------------------" //
              vcat top (formatExample lma sensemap sensestat)
    putStrLn (render doc)
   -}


main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                  )
    listSenseExample pp
