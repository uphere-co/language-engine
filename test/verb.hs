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
import           Data.Maybe                        (catMaybes,mapMaybe)
import qualified Data.Text                  as T   (intercalate,unpack)
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
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           Data.BitreeZipper
--
import           SRL.Feature
import           SRL.Feature.Verb


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



          -- , "The man, it seems, has a Lichtenstein corporation, licensed in Libya and sheltered in the Bahamas. Coke introduced a caffeine-free sugared cola based on its original formula in 1983. But there were fewer price swings than expected. Two big stocks involved in takeover activity saw this."

-- showVoice :: (PennTree,S.Sentence) -> IO ()
-- showVoice (pt,sent) = mapM_ print (voice (pt,sent) )

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
      let lst = zip pts sents
      flip mapM_ lst $ \x@(pt,sent) -> do
        -- print (mkLemmaMap sent)
        -- print (voice x)
        putStrLn "\n\n======================================="
        T.IO.putStrLn txt
        putStrLn "---------------------------------------"
        T.IO.putStrLn (prettyPrint 0 pt)
        print (getVerbProperty x)


main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ False )
                   . ( depparse .~ False ) -- True )
                   . ( constituency .~ True )
                   . ( ner .~ False )
    pp <- prepare pcfg
    mapM_ (process pp) testtxt
