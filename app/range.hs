{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative               (many,(*>))
import           Control.Lens               hiding (levels)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Foldable                     (toList)
import           Data.Graph                        (buildG,dfs,dff,scc,topSort)
import           Data.List                         (zip4)
import qualified Data.IntMap                as IM
import           Data.Maybe                        (fromJust,fromMaybe,mapMaybe)
import qualified Data.Sequence              as Seq
import           Data.Text                         (Text)
import qualified Data.Text                  as T   (intercalate,unpack)
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                (fromGregorian)
import           Data.Tree                         (levels)
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
                             , _corenlp_dep  :: Dependency
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
  -- print dep
  let Dependency root nods edgs' = dep
      bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
  let searchtree = head (dfs (buildG bnds edgs) [root])
      levelMap = IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]
  -- print levelMap



  let sampletree1 = fmap (annotateLevel levelMap) sampletree

  mapM_forNode (print . getLeaves) sampletree1


annotateLevel levelmap (n,txt) = (IM.lookup n levelmap,txt)

mapM_forNode f x@(PN c xs) = f x >> mapM_ (mapM_forNode f) xs
mapM_forNode f (PL t x ) = return ()



--    where f xs n = (\n -> map (n,)) xs 
-- getpaths 

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
          -- deps' = map (^. S.
      let cpts = mapMaybe (^.S.parseTree) sents
          pts = map decodeToPennTree cpts
          rs = map (\(i,((pt,tr,dep),pr)) -> (i,SentInfo pt tr dep,pr))
             . merge (^.inst_tree_id) (zip3 pts trs deps)
             $ props
      -- mapM_ action rs
      liftIO $ mapM_ showParseTree rs -- (head rs)

      -- liftIO $ mapM_ testsent sents


testsent sent = do
  let Just g = sent ^. S.basicDependencies
  print $ sent ^.. S.token . traverse . TK.word
  mapM_ print $ toList (g^.DG.node)
