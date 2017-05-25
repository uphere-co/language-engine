{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative               (many,(*>))
import           Control.Lens
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Text                         (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                (fromGregorian)
import           Language.Java              as J
import           System.Environment                (getEnv)
import           Text.ProtocolBuffers.WireMessage  (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.ParseTree as PT
import           NLP.Parser.PennTreebankII
import           NLP.Type.PennTreebankII

main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  
  putStrLn "main"
  let f1 = "/scratch/wavewave/Penn-tbank/MRG/WSJ/00/WSJ_0026.MRG"
  txt <- TIO.readFile f1
  let f2 = "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/wsj_0026.mrg"
  txt2 <- TIO.readFile f2
  void . runEitherT $ do
    lst1 <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt
    lst2 <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt2
    liftIO $ do
      mapM_ print lst1
      putStrLn "--------------------"
      mapM_ print lst2
      putStrLn "==================="
      print (lst1 == lst2)
      main2 clspath


processDoc :: J ('Class "edu.stanford.nlp.pipeline.Annotation")
           -> IO (Either String D.Document) 
processDoc ann = do
  bstr <- serializeDoc ann
  let lbstr = BL.fromStrict bstr
  return $ fmap fst (messageGet lbstr :: Either String (D.Document,BL.ByteString))

      
main2 clspath = do
  
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/MASC1_textfiles/written/wsj_0026.txt"
  
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = def & ( tokenizer .~ True )
                   . ( words2sentences .~ True )
                   . ( postagger .~ True )
                   . ( lemma .~ True )
                   . ( sutime .~ True )
                   . ( depparse .~ False )
                   . ( ner .~ False )
    pp <- prepare pcfg
    let doc = Document txt (fromGregorian 2017 4 17) 
    ann <- annotate pp doc
    rdoc <- processDoc ann
    case rdoc of
      Left e -> print e
      Right d -> print d

