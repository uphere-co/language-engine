{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.PropBankMatch where

import           Control.Applicative             (many)
import           Control.Lens
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Either      (EitherT,hoistEither)
import qualified Data.Attoparsec.Text       as A
import           Data.Foldable                   (toList)
import           Data.List (intercalate)
import           Data.Maybe                      (fromJust,mapMaybe,listToMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar              (fromGregorian)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as CS
import qualified CoreNLP.Simple.Type                   as S
import qualified CoreNLP.Simple.Type.Simplified        as S
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
--
import           SRL.Util
--

data SentenceInfo = SentInfo { _corenlp_sent :: CS.Sentence
                             , _corenlp_tree :: PennTree
                             , _propbank_tree :: PennTree
                             , _corenlp_dep  :: S.Dependency
                             }
                  deriving Show

makeLenses ''SentenceInfo


data MatchedArgNode
  = MatchedArgNode { _mn_node :: (Range,Node)
                   , _mn_trees :: [(Range,PennTreeIdx)]
                   }
  deriving Show

makeLenses ''MatchedArgNode

data MatchedArgument
  = MatchedArgument { _ma_argument :: Argument
                    , _ma_nodes :: [MatchedArgNode]
                    }
  deriving Show

makeLenses ''MatchedArgument

data MatchedInstance
  = MatchedInstance { _mi_instance :: Instance
                    , _mi_arguments :: [MatchedArgument]
                    }
  deriving Show

makeLenses ''MatchedInstance


termRangeForAllNode :: PennTreeGen c (Int,t) -> [Range]
termRangeForAllNode x@(PN _ ys) = termRange x : concatMap termRangeForAllNode ys
termRangeForAllNode (PL (i,_)) = [(i,i)]


adjustIndex :: [Int] -> Int -> Either Int Int
adjustIndex xs n = let m = length (filter (<n) xs)
                   in if n `elem` xs then Left (n-m) else Right (n-m)

adjustIndexFromTree :: PennTree -> Int -> Either Int Int
adjustIndexFromTree tr =
  let itr = mkIndexedTree tr
      excl = map (^._1) (findNoneLeaf itr)
  in adjustIndex excl 


maximalEmbeddedRange :: PennTreeGen c (Int,t) -> Range -> [(Range,PennTreeIdxG c t)]
maximalEmbeddedRange tr r = go (termRangeTree tr)
  where go y@(PN (r1,_) xs) = if r1 `isInsideR` r then [(r1,y)] else concatMap go xs
        go y@(PL (n,_)) = if n `isInside` r then [((n,n),y)] else []


matchR :: Range -> PennTreeIdxG c t -> Maybe (PennTreeIdxG c t)
matchR r0 y@(PN (r,_) xs)
  | r0 == r = Just y 
  | otherwise = listToMaybe (mapMaybe (matchR r0) xs)
matchR (b,e) x@(PL (n,_))
  | b == n && e == n = Just x
  | otherwise = Nothing

matchArgNodes :: (PennTree,PennTree) -> Argument -> [MatchedArgNode]
matchArgNodes (pt,tr) arg = do
  n <- arg ^. arg_terminals
  let nd = fromJust (findNode n tr)
  let adjf = adjustIndexFromTree tr
      adjrange (x,y) = case (adjf x, adjf y) of
                         (Left b,Left e) -> if b == e then [] else [(b,e)]
                         (Left b,Right e) -> [(b,e)]
                         (Right b,Left e) -> [(b,e-1)]
                         (Right b,Right e) -> [(b,e)]
  rng <- (adjrange . termRange . snd) nd
  let ipt = (mkIndexedTree . getADTPennTree) pt
      zs = maximalEmbeddedRange ipt rng
  return MatchedArgNode { _mn_node = (rng,n), _mn_trees = zs }


matchArgs :: (PennTree,PennTree) -> Instance -> [MatchedArgument]
matchArgs (pt,tr) pr
  = [ MatchedArgument { _ma_argument = a, _ma_nodes = matchArgNodes (pt,tr) a } | a <- pr^.inst_arguments ]


matchInstances :: (PennTree,PennTree) -> [Instance] -> [MatchedInstance]
matchInstances (pt,tr) insts
  = [ MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (pt,tr) inst }
      | inst <- insts ]


printMatchedNode :: MatchedArgNode -> IO ()
printMatchedNode x = do
  TIO.putStrLn $ T.pack (show (x^.mn_node._1)) <> ":"
  print (x^.mn_trees)


printMatchedArg :: MatchedArgument -> IO ()
printMatchedArg x = do
  putStrLn $ show (x^.ma_argument.arg_label)
  mapM_ printMatchedNode (x^.ma_nodes)


printMatchedInst :: MatchedInstance -> IO ()
printMatchedInst x = do
  TIO.putStrLn (x^.mi_instance.inst_lemma_type)
  putStrLn "---"  
  mapM_ printMatchedArg (x^.mi_arguments)
  putStrLn "---"  

findRelNode :: [MatchedArgument] -> Int
findRelNode args =
    let a1 = headf $ filter (\a -> a ^. ma_argument.arg_label == Relation) args
    in headf (a1^..ma_nodes.traverse.mn_node._1._1)
  where headf [] = error ("findRelNode: " ++ intercalate "\n" (map show args))
        headf (x:_) = x


propbank :: (FilePath,FilePath,IsOmit) -> EitherT String IO ([PennTree],[Instance])
propbank (pennfile,propfile,omit) =  do
  txt <- liftIO $ TIO.readFile pennfile
  trs <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt
  props <- liftIO $ parseProp omit <$> TIO.readFile propfile
  return (trs,props)


mkDocFromPennTree :: PennTree -> S.Document
mkDocFromPennTree = flip S.Document (fromGregorian 2017 4 17)
                  . T.intercalate " "
                  . map snd
                  . filter (\(t :: Text,_) -> t /= "-NONE-")
                  . getLeaves  


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
  -- mapM_ printMatchedInst $ matchInstances (pt,tr) prs


