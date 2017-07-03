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
import qualified Data.Text.IO               as TIO
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
import           SRL.Feature.Voice


showVoice :: (PennTree,S.Sentence) -> IO ()
showVoice (pt,sent) = do
  let lst = catMaybes (sent ^.. S.token . traverse . TK.originalText . to (fmap cutf8))
  TIO.putStrLn "---------- VOICE -----------------"
  TIO.putStrLn $ T.intercalate " " lst
  TIO.putStrLn (prettyPrint 0 pt)       
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
  let getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (VBN,txt) annot)
                    -> putStrLn (show n ++ ": " ++  T.unpack txt ++ ": " ++ show (isPassive z))
                  _ -> return ()
  mapM_ testf (mkBitreeZipper [] lemmapt)


main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    let txt = "He was fined $25,000. He will be fined $25,000. He has been fined $25,000. The move had been widely expected. The man, it seems, has a Lichtenstein corporation, licensed in Libya and sheltered in the Bahamas. Coke introduced a caffeine-free sugared cola based on its original formula in 1983. But there were fewer price swings than expected. Two big stocks involved in takeover activity saw this."
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ False )
                   . ( depparse .~ False ) -- True )
                   . ( constituency .~ True )
                   . ( ner .~ False )
    pp <- prepare pcfg
    let doc = Document txt (fromGregorian 2017 4 17)
    ann <- annotate pp doc
    rdoc <- protobufDoc ann
    case rdoc of
      Left e -> print e
      Right d -> do
        let sents = d ^.. D.sentence . traverse
            cpts = mapMaybe (^.S.parseTree) sents
            pts = map decodeToPennTree cpts
        mapM_ showVoice (zip pts sents)
