{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative               (many,(*>))
import           Control.Lens               hiding (levels)
import           Control.Monad                     (void,when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8      as B
import           Data.Default
import           Data.Foldable                     (toList)
import           Data.List                         (foldl',zip4)
import           Data.Monoid                       ((<>))
import qualified Data.IntMap                as IM
import           Data.Maybe                        (catMaybes,mapMaybe)
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
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
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

showMatchedInstance :: (Int,SentenceInfo,[Instance]) -> IO ()
showMatchedInstance (_i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      terms = map (^._2) . toList $ pt
  TIO.putStrLn "================="
  TIO.putStrLn "PropBank"
  TIO.putStrLn $ prettyPrint 0 tr
  TIO.putStrLn "-----------------"
  TIO.putStrLn "CoreNLP"  
  TIO.putStrLn $ prettyPrint 0 pt
  TIO.putStrLn "-----------------"            
  TIO.putStrLn (T.intercalate " " terms)
  TIO.putStrLn "-----------------"
  mapM_ printMatchedInst $ matchInstances (pt,tr) prs


showFeaturesForArgNode :: SentenceInfo -> Int -> Argument -> MatchedArgNode -> IO ()
showFeaturesForArgNode sentinfo predidx arg node = 
  when (arg ^. arg_label /= "rel")  $ do
    print (arg ^. arg_label)
    let rngs = node ^.. mn_trees . traverse . _1
    let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
        dep = sentinfo^.corenlp_dep
        parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
        opaths = map parseTreePath parsetrees
        paths = map (simplifyPTP . parseTreePath) parsetrees
        
        heads = map (\rng -> headWord =<< matchR rng (headWordTree dep ipt)) rngs
    mapM_ print (zip4 rngs opaths paths heads)

    
showFeaturesForArg :: SentenceInfo -> Int -> MatchedArgument -> IO ()
showFeaturesForArg sentinfo predidx arg = 
  mapM_ (showFeaturesForArgNode sentinfo predidx (arg^.ma_argument)) (arg^.ma_nodes)

  
showFeaturesForInstance :: SentenceInfo -> MatchedInstance -> IO ()
showFeaturesForInstance sentinfo inst = do
  print (inst ^. mi_instance.inst_lemma_type)
  let predidx = findRelNode (inst^.mi_arguments)
  mapM_ (showFeaturesForArg sentinfo predidx) (inst^.mi_arguments)


showVoice :: (PennTree,S.Sentence) -> IO ()
showVoice (pt,sent) = do
  TIO.putStrLn "---------- VOICE -----------------"
  -- TIO.putStrLn $ T.intercalate " " lst                       
  let ipt = mkPennTreeIdx pt
  -- TIO.putStrLn (prettyPrint 0 pt)     
  let lemmamap =  foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))
      lemmapt = lemmatize lemmamap ipt
  let getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> putStrLn (show n ++ ": " ++  T.unpack txt ++ ": " ++ show (isPassive z))
                  _ -> return ()
  mapM_ testf (mkTreeZipper [] lemmapt)



showFeatures :: (Int,SentenceInfo,[Instance]) -> IO ()
showFeatures (_i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
  showFeaturesForInstance sentinfo (head insts)
  showVoice (pt,sentinfo^.corenlp_sent)
  
main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
  
    void . runEitherT $ do
      (trs,props) <- propbank
      rdocs <- liftIO $ do
        -- txt <- liftIO $ TIO.readFile "/scratch/wavewave/MASC/Propbank/MASC1_textfiles/written/wsj_0026.txt"
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
      deps <- hoistEither $ mapM sentToDep sents
      let cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          rs = map (\(i,((pt,tr,dep,sent),pr)) -> (i,SentInfo sent pt tr dep,pr))
             . merge (^.inst_tree_id) (zip4 pts trs deps sents)
             $ props
      liftIO $ mapM_ (showMatchedInstance <> showFeatures) rs

