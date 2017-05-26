{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative               (many,(*>))
import           Control.Arrow                     ((***))
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
import           SRL.Util

adjustIndexFromTree :: PennTree -> Int -> Int
adjustIndexFromTree tr =
  let itr = mkIndexedTree tr
      rs = termRangeForAllNode itr
      excl = map (^._2._1) (findNoneLeaf itr)
  in adjustIndex excl 


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
        let docs = map mkDocFromPennTree trs -- Document txt (fromGregorian 2017 4 17) 
        anns <- mapM (annotate pp) docs
        rdocs <- mapM protobufDoc anns
        return rdocs
      ds <- mapM hoistEither rdocs

      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          cpts = mapMaybe (^.S.parseTree) sents
          pts = map convertPennTree cpts

      let rs = merge (^.inst_tree_id) (zip pts trs) props

      liftIO $ findMatchedNode (head rs)



clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

findMatchedNode (i,((pt,tr),pr)) = do
  let terms =  toList pt

  TIO.putStrLn $ prettyPrint 0 pt
  TIO.putStrLn (T.intercalate " " terms)
  TIO.putStrLn "================="
  let pr0 = pr !! 1
      args = pr0 ^. inst_arguments
      arg0 = args !! 1
  print arg0
  
  let nds = map (flip findNode tr) (arg0 ^. arg_terminals)
      nd = fromJust (head nds)

  
  let adjf = adjustIndexFromTree tr

      rng = ((adjf *** adjf) . termRange . snd) nd

  putStrLn . formatRngText terms $ rng
  -- print rng
  --  print (clippedText rng terms)
  putStrLn "-----------"
  
  let xs = termRangeForAllNode (mkIndexedTree pt)

  mapM_ (putStrLn . formatRngText terms) xs

  --


  

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)
  
  -- mapM_ findNode 
