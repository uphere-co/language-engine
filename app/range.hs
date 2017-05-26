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
import           Data.Foldable                     (toList)
import           Data.List                         (zip4)
import           Data.Maybe                        (fromJust,fromMaybe,mapMaybe)
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
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Match
import           SRL.Util


mkDocFromPennTree :: PennTree -> Document
mkDocFromPennTree = flip Document (fromGregorian 2017 4 17)
                  . T.intercalate " "
                  . map snd
                  . filter (\(t :: Text,_) -> t /= "-NONE-")
                  . getLeaves  

propbank :: EitherT String IO ([PennTree],[Instance])
propbank =  do
  props <- liftIO $ parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/wsj_0026.prop"
  txt <- liftIO $ TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/wsj_0026.mrg"
  trs <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt
  return (trs,props)


main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
  
    void . runEitherT $ do
      (trs,props) <- propbank
      rdocs <- liftIO $ do
        txt <- liftIO $ TIO.readFile "/scratch/wavewave/MASC/Propbank/MASC1_textfiles/written/wsj_0026.txt"
        let pcfg = def & ( tokenizer .~ True )
                       . ( words2sentences .~ True )
                       . ( postagger .~ True )
                       . ( lemma .~ True )
                       . ( sutime .~ True )
                       . ( depparse .~ False )
                       . ( constituency .~ True )
                       . ( ner .~ False )
        pp <- prepare pcfg
        let docs = map mkDocFromPennTree trs
        anns <- mapM (annotate pp) docs
        rdocs <- mapM protobufDoc anns
        return rdocs
      ds <- mapM hoistEither rdocs
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          cpts = mapMaybe (^.S.parseTree) sents
          pts = map convertPennTree cpts
          rs = merge (^.inst_tree_id) (zip pts trs) props
      let action (i,((pt,tr),pr)) = liftIO $ do
            let terms = toList pt
            TIO.putStrLn "================="
            TIO.putStrLn $ prettyPrint 0 pt
            TIO.putStrLn (T.intercalate " " terms)
            print pr
            TIO.putStrLn "-----------------"
            mapM_ print $ findMatchedNode ((pt,tr),pr)
      mapM_ action rs
      
