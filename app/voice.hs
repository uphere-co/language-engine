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
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Either                       (lefts)
import           Data.Foldable                     (toList)
import           Data.Function                     (on)
import           Data.List                         (sortBy,zip4)
import           Data.Monoid                       ((<>))
import qualified Data.IntMap                as IM
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
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Feature
import           SRL.PropBankMatch
import           SRL.Util
import           SRL.VoiceIdentify

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


showVoice :: (Int,SentenceInfo,[Instance]) -> IO ()
showVoice (i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      apt = getADTPennTree pt
      atree = ancestorTree apt
  TIO.putStrLn (prettyPrint 0 pt)     
  print $ fmap (\(xs,y) -> (lefts (map getTag xs),y)) atree
  print $ fmap (\(xs,y) -> (map getTag xs,y)) (siblings atree)
  -- print (siblings atree)

  
showMatchedInstance :: (Int,SentenceInfo,[Instance]) -> IO ()
showMatchedInstance (i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      terms = toList pt
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

findRelNode :: [MatchedArgument] -> Int
findRelNode args =
  let arg = head $ filter (\arg -> arg ^. ma_argument.arg_label == "rel") args
  in head (arg^..ma_nodes.traverse.mn_node._1._1)

showFeaturesForArgNode :: SentenceInfo -> Int -> Argument -> MatchedArgNode -> IO ()
showFeaturesForArgNode sentinfo predidx arg node = 
  when (arg ^. arg_label /= "rel")  $ do
    print (arg ^. arg_label)
    let rngs = node ^.. mn_trees . traverse . _1
    let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
        dep = sentinfo^.corenlp_dep
        parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
        paths = map parseTreePath parsetrees
        headWordTree = headWord dep ipt
        heads = map (\rng -> pickHeadWord =<< matchR rng (headWord dep ipt)) rngs
    mapM_ print (zip3 rngs paths heads)

safeHead [] = Nothing
safeHead (x:_) = Just x

pickHeadWord  = safeHead . map snd . sortBy (compare `on` fst)
              . mapMaybe (\(_,(_,(ml,t))) -> (,) <$> ml <*> pure t) . getLeaves 
    
showFeaturesForArg :: SentenceInfo -> Int -> MatchedArgument -> IO ()
showFeaturesForArg sentinfo predidx arg = 
  mapM_ (showFeaturesForArgNode sentinfo predidx (arg^.ma_argument)) (arg^.ma_nodes)

  
showFeaturesForInstance :: SentenceInfo -> MatchedInstance -> IO ()
showFeaturesForInstance sentinfo inst = do
  print (inst ^. mi_instance.inst_lemma_type)
  let predidx = findRelNode (inst^.mi_arguments)
  mapM_ (showFeaturesForArg sentinfo predidx) (inst^.mi_arguments)

showFeatures :: (Int,SentenceInfo,[Instance]) -> IO ()
showFeatures (i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
  showFeaturesForInstance sentinfo (head insts)
  
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
      deps <- hoistEither $ mapM sentToDep sents
      let cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          rs = map (\(i,((pt,tr,dep),pr)) -> (i,SentInfo pt tr dep,pr))
             . merge (^.inst_tree_id) (zip3 pts trs deps)
             $ props
      -- liftIO $ mapM_ (showMatchedInstance <> showFeatures) rs
      liftIO $ showVoice (head rs)
