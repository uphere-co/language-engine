{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
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
import           OntoNotes.App.Load
import           OntoNotes.App.Serializer
import           OntoNotes.Corpus.Load      hiding (prepare)
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.Type.Sense
import           OntoNotes.Type.SenseInventory


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

{- 
errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()
-}

text_buy1
  = [ "The family bought a new car."
    , "According to his official biography, he bought his first bison in 1976."
    , "Should I fire him because he bought his degree from the internet instead of attending a regular university?"
    , "She wanted to buy his love with her dedication to him and his work."
    , "Money can't buy me love."
    , "My mom's boyfriend would try to buy my trust by taking us out to eat."
    , "Novell, under pressure, will buy back its stock."
    , "I think my used car was bought back once by the manufacturer and the dealer didn't tell me when I bought it."
    , "Businessmen opposed to the Vietnam war tried to buy time for anti-war spots on a radio station."
    , "A New Hampshire Poll and Two Dollars Will Buy You a Cup of Coffee."
    , "No amount of money can buy you safety."
    , "The insurance premium you forego will buy you a pretty good road bike."
    ]

text_examine3
  = [ "Candidates must pass a qualifying test which examines for general and specific knowledge."
    , "A psychiatrist was examined on the mental state of the defendant."
    , "The prosecutor examined the witness under oath."
    ]
 
text_monopolize1
  = [ "China has charged that the U.S. monopolizes the Internet."
    , "He monopolized the use of the goods and services for his own interest."
    , "Why must she always monopolize the conversation?"
    ]
        
text_forge1
  = [ "Forge the silver into a bowl."
    , "Forge a pair of tongues."
    , "Forge the metal into a sword."
    , "The two governments met to forge an alliance."
    , "Childhood abuse can forge a person's outlook on life."
    , "Ideas that were forged out of reality are most beneficial."
    , "A family insurance plan should be forged out of love."
    ]

text_round8
  = [ "I've rounded up some simple tips and tutorials for the conference."
    , "Kay has rounded up a bunch of great articles on our tax system."
    , "The police rounded up all the suspects."
    ]

      



listSenseExample basedir pp = do
  (_ludb,sensestat,_semlinkmap,sensemap,ws,_wndb) <- loadAllexceptPropBank

  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)
  let merged = mergeStatPB2Lemma ws

  withFile (basedir </> "error.log") WriteMode $ \h_err -> 
    forM_ merged $ \(lma,f) -> do
        
      let lst = -- filter (\x-> (x^._1 == "buy-v") || (x^._1 == "examine-v") || (x^._1 == "monopolize-v") ||
                --              (x^._1 == "forge-v") || (x^._1 == "round-v") 
                --        ) $
                  formatExample lma sensemap sensestat
      forM_ lst $ \(lmav,sense,txts') -> do
        T.IO.hPutStrLn stdout lmav
        
        -- print (lmav,sense,txts')
        let bname = T.unpack (lmav <> "_" <> sense)
        let txts = if | bname == "buy-v_1"        -> text_buy1
                      | bname == "examine-v_3"    -> text_examine3
                      | bname == "monopolize-v_1" -> text_monopolize1
                      | bname == "forge-v_1"      -> text_forge1
                      | bname == "round-v_8"      -> text_round8
                      | otherwise                 -> txts'

        -- print txts
        
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
