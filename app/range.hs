{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           SRL.Feature
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


data SentenceInfo = SentInfo { _corenlp_tree :: PennTree
                             , _propbank_tree :: PennTree
                             , _corenlp_dep  ::  Maybe Dependency
                             }
                  deriving Show

makeLenses ''SentenceInfo

showMatchedInstance :: (Int,SentenceInfo,[Instance]) -> IO ()
showMatchedInstance (i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      terms = toList pt
  TIO.putStrLn "================="
  TIO.putStrLn $ prettyPrint 0 pt
  TIO.putStrLn "-----------------"
  TIO.putStrLn $ prettyPrint 0 tr
  TIO.putStrLn "-----------------"            
  TIO.putStrLn (T.intercalate " " terms)
  TIO.putStrLn "-----------------"
  mapM_ printMatchedInst $ matchInstances (pt,tr) prs
  TIO.putStrLn "-----------------"
  print $ getADTPennTree pt 

showParseTree :: (Int,SentenceInfo,[Instance]) -> IO ()
showParseTree (i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      dep = sentinfo^.corenlp_dep
      lst =  matchInstances (pt,tr) prs
      lst0 = snd (head lst) !! 0
      lst1 = snd (head lst) !! 1
      (tgt,_) = head (snd (head (snd lst1)))
  print lst0
  print tgt
  let sampletree = mkPennTreeIdx pt
  -- print (getADTPennTree pt)
  TIO.putStrLn (prettyPrint 0 pt)
  let parsetree@(mhead,ptpath_s,ptpath_t) = parseTreePathFull (5,(13,36)) sampletree
  print $ fmap phraseType mhead
  print $ map phraseType ptpath_s
  print $ map phraseType ptpath_t
  print $ parseTreePath parsetree
  print dep


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
                       . ( sutime .~ False )
                       . ( depparse .~ True )
                       . ( constituency .~ True )
                       . ( ner .~ False )
        pp <- prepare pcfg
        let docs = map mkDocFromPennTree trs
        anns <- mapM (annotate pp) docs
        rdocs <- mapM protobufDoc anns
        return rdocs
      ds <- mapM hoistEither rdocs
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          deps = map sentToDep sents
          cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          rs = map (\(i,((pt,tr,dep),pr)) -> (i,SentInfo pt tr dep,pr))
             . merge (^.inst_tree_id) (zip3 pts trs deps)
             $ props
      -- mapM_ action rs
      liftIO $ mapM_ showParseTree rs
      
