{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels)
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Data.List                         (foldl')
import qualified Data.IntMap                as IM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Time.Calendar                (fromGregorian)
import           Language.Java              as J
import           System.Environment                (getEnv)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Util
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           Data.BitreeZipper
import           Text.Format.Tree
--
import           SRL.Feature
import           SRL.Feature.Dependency
import           SRL.Feature.Verb
import           SRL.Format
import           SRL.Type


testtxt = [ "He was fined $25,000."                -- past    simple             passive
          , "He will be fined $25,000."            -- present simple             passive
          , "He has been fined $25,000."           -- present perfect            passive
          , "The move had been widely expected."   -- past    perfect            passive
          , "I am floating."                       -- present progressive        active  
          , "I am studying with Maria."            -- present progressive        active
          , "We eat lunch."                        -- present simple             active
          , "We are eating lunch."                 -- present progressive        active
          , "We are not eating lunch right now."   -- present progressive        active
          , "It's not done yet."                   -- present simple             passive
          , "It's done."                           -- present simple             passive
          , "It's rarely noted."                   -- present simple             passive 
          , "I have been watching TV."             -- present perfectprogressive active
          , "The book had not been noticed."       -- past    perfect            passive
          , "I have done the job."                 -- present perfect            active
          , "I haven't done the job."              -- present perfect            active
          , "I had done the job at that time."     -- past    perfect            active
          ]

testtxt2 = [ "President Donald Trump said he’s actively considering a breakup of giant Wall Street banks, giving a push to efforts to revive a Depression-era law separating consumer and investment banking. "
           , "Carmakers, rideshare services and tech companies are teaming up in an increasingly complex series of alliances."
           , "But that tenet was undone in 1999, a move that’s been blamed by some for the 2008 market crash. Bringing that system back has won the support of politicians as diverse as President Donald Trump and Senator Elizabeth Warren, the Massachusetts Democrat who’s been one of Wall Street’s toughes."
           , "President Donald Trump on Monday dismissed widespread reports that his administration is riven by discord, saying he is sticking by his polarizing chief strategist, Steve Bannon, calling him a \"very decent guy\" who is getting a \"bad rap.\""
           , "The man, it seems, has a Lichtenstein corporation, licensed in Libya and sheltered in the Bahamas. Coke introduced a caffeine-free sugared cola based on its original formula in 1983. But there were fewer price swings than expected. Two big stocks involved in takeover activity saw this."             
           ]

testtxt3 = [ "I will go to school."
           , "I could have done it."
           , "Can I ask you something?"
           , "Can I ask you a favor?"
           ]

    
process pp txt = do
  let doc = Document txt (fromGregorian 2017 4 17)
  ann <- annotate pp doc
  rdoc <- protobufDoc ann
  case rdoc of
    Left e -> print e
    Right d -> do
      let sents = d ^.. D.sentence . traverse
          cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          Right deps = mapM sentToDep sents
      let lst = zip (zip pts sents) deps
      flip mapM_ lst $ \((pt,sent),dep) -> do
        let tkns = zip [0..] (getTKTokens sent)
            tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
            lmap= mkLemmaMap sent
        print pt
        {- 
        putStrLn "\n\n======================================="
        T.IO.putStrLn txt
        putStrLn "---------------------------------------"
        T.IO.putStrLn (prettyPrint 0 pt)
        putStrLn "---------------------------------------"
        let vps = verbPropertyFromPennTree lmap pt
        mapM_ (putStrLn . formatVerbProperty) vps 
        putStrLn "---------------------------------------"
        -- sentStructure 
        let vtree = verbTree vps . depLevelTree dep . lemmatize lmap . mkAnnotatable . mkPennTreeIdx $ pt
        mapM_ (T.IO.putStrLn . formatBitree (^._2.to (showVerb tkmap))) vtree
        putStrLn "---------------------------------------------------------------"
        -- (T.IO.putStrLn . prettyPrint 0) ptr
        -}



main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ False )
                   . ( depparse .~ True )
                   . ( constituency .~ True )
                   . ( ner .~ False )
    pp <- prepare pcfg
    mapM_ (process pp) testtxt2
