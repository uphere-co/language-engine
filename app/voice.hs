{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative               (many,(*>))
import           Control.Lens               hiding (levels)
import           Control.Monad                     (void,when,(>=>))
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text       as A
import           Data.Bifoldable
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Either                       (lefts)
import           Data.Foldable                     (toList)
import           Data.Function                     (on)
import           Data.List                         (foldl',sortBy,zip4)
import           Data.Monoid                       ((<>))
import qualified Data.IntMap                as IM
import           Data.Maybe                        (catMaybes,fromJust,fromMaybe,mapMaybe)
import qualified Data.Sequence              as Seq
import           Data.Text                         (Text)
import qualified Data.Text                  as T   (intercalate,unpack)
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                (fromGregorian)
import           Language.Java              as J
import           System.Environment                (getEnv)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.ParseTree as PT
import qualified CoreNLP.Proto.CoreNLPProtos.DependencyGraph       as DG
import qualified CoreNLP.Proto.CoreNLPProtos.DependencyGraph.Node  as DN
import qualified CoreNLP.Proto.CoreNLPProtos.DependencyGraph.Edge  as DE
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature
import           SRL.PropBankMatch
import           SRL.Util
import           SRL.IdentifyVoice


showVoice :: (PennTree,S.Sentence) -> IO ()
showVoice (pt,sent) = do
  let ipt = mkPennTreeIdx pt
  TIO.putStrLn (prettyPrint 0 pt)     
  let lemmamap =  foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))
      lemmapt = lemmatize lemmamap ipt
  print lemmapt
  let getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> putStrLn (show n ++ ": " ++  T.unpack txt ++ ": " ++ show (isPassive z))
                  x -> return ()
  mapM_ testf (mkTreeZipper [] lemmapt)



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
