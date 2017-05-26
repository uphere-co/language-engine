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
import           Data.Text                         (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                (fromGregorian)
import           Language.Java              as J
import           System.Environment                (getEnv)
import           Text.ProtocolBuffers.WireMessage  (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.ParseTree as PT
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Util

adjustIndexFromTree :: PennTree -> Int -> Int --  [(Int,Int)] 
adjustIndexFromTree tr =
  let itr = mkIndexedTree tr
      rs = termRangeForAllNode itr
      excl = map (^._2._1) (findNoneLeaf itr)
  in adjustIndex excl 

 
processDoc :: J ('Class "edu.stanford.nlp.pipeline.Annotation")
           -> IO (Either String D.Document) 
processDoc ann  = do
  bstr <- serializeDoc ann
  let lbstr = BL.fromStrict bstr
  return $ fmap fst (messageGet lbstr :: Either String (D.Document,BL.ByteString))


propbank :: EitherT String IO ([PennTree],[Instance])
propbank =  do
  props <- liftIO $ parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/wsj_0026.prop"
  txt <- liftIO $ TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/wsj_0026.mrg"
  -- let xs = T.lines txt
  --    txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
  trs <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt
  return (trs,props)
{-     Left err -> print err
    Right trs -> do
      mapM_ showSentenceProp (merge (^.inst_tree_id) trs props) -- lst
-}

main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do 
  
    void . runEitherT $ do
      rdoc <- liftIO $ do 
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
        let doc = Document txt (fromGregorian 2017 4 17) 
        ann <- annotate pp doc
        rdoc <- processDoc ann
        return rdoc
      d <- hoistEither rdoc 
      let sents = d ^.. D.sentence . traverse
          Just newsents = mapM (convertSentence d) sents
          cpts = mapMaybe (^.S.parseTree) sents
          pts = map convertPennTree cpts




      (trs,props) <- propbank
      let rs = merge (^.inst_tree_id) (zip pts trs) props

      liftIO $ process (head rs)
      -- liftIO $ print (length rs)

{-
      liftIO $ do
        TIO.putStrLn (pennTreePrint 0 pt)
        let rs = termRangeForAllNode (mkIndexedTree pt)
        print rs
-}


process (i,((pt,tr),pr)) = do
  print i
  print pt
  print tr
  print pr

  let pr0 = pr !! 1
      args = pr0 ^. inst_arguments
      arg0 = args !! 1
  print arg0
  let nds = map (flip findNode tr) (arg0 ^. arg_terminals)
      nd = fromJust (head nds)

  let adjf = adjustIndexFromTree tr

      rng =  termRange (snd nd)
  print ((adjf *** adjf) rng)

  print $ termRangeForAllNode (mkIndexedTree pt)

  mapM_ print . toList . mkIndexedTree $ pt
  -- mapM_ findNode 
