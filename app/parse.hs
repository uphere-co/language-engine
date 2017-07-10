{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens                  hiding ((<.>))
import           Control.Monad
import           Control.Monad.Loops                  (unfoldM)
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.Attoparsec.Text         as A
import           Data.Bifoldable
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Default
import           Data.Foldable                        (toList,traverse_)
import qualified Data.IntMap                  as IM
import           Data.List                            (intercalate,sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence                as Seq
import           Data.Text                            (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import           Data.Time.Calendar
import           Language.Java                as J
import           System.Directory.Tree
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import          CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type          
import           NLP.Type.PennTreebankII
import           NLP.Parser.PennTreebankII


getTerms :: PennTree -> [Text]
getTerms = map snd . filter (\(t,_) -> t /= "-NONE-") . toList 


parseOntoNotesPennTree :: FilePath -> IO (Either String [PennTree])
parseOntoNotesPennTree f = fmap (A.parseOnly (A.many1 (A.skipSpace *> pnode))) (T.IO.readFile f)


      
serializeLemma pp trs h_lemma = do
  let tss = map getTerms trs
      ts = map (T.intercalate " ") tss
      -- ntxt = T.intercalate "\n\n" ts
      docs = map (flip Document (fromGregorian 1990 1 1)) ts -- ntxt
  anns <- traverse (annotate pp) docs
  rdocs' <- traverse protobufDoc anns
  let rdocs = sequenceA rdocs'
  case rdocs of
    Left err -> print err
    Right ds -> do
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          lmap= map (map (_2 %~ unLemma) . IM.toList . mkLemmaMap) sents
      BL.hPutStrLn h_lemma (encode lmap)


serializePennTreeDep pp trs (h_ud,h_tr)= do
  let tss = map getTerms trs
      ts = map (T.intercalate " ") tss
      -- ntxt = T.intercalate "\n\n" ts
      docs = map (flip Document (fromGregorian 1990 1 1)) ts -- ntxt
  anns <- traverse (annotate pp) docs
  rdocs' <- traverse protobufDoc anns
  let rdocs = sequenceA rdocs'
  case rdocs of
    Left err -> print err
    Right ds -> do
      let sents = map (flip Seq.index 0 . (^. D.sentence)) ds
          ntrs = map decodeToPennTree (mapMaybe (^.S.parseTree) sents)
          edeps = mapM sentToDep sents
      case edeps of
        Left err -> print err
        Right deps -> do
          BL.hPutStrLn h_ud (encode deps)
          BL.hPutStrLn h_tr (encode ntrs)


errorHandler h_err msg action = do
  r <- try action 
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg
    _ -> return ()


convertTop (PN _ xs) = PN "ROOT" xs

filterNML x@(PN NML xs) = [x]
filterNML (PN c xs) = concatMap filterNML xs
filterNML (PL _) = []

mergeHyphen :: State [(POSTag,Text)] (Maybe [(POSTag,Text)])
mergeHyphen = fmap (fmap reverse) (go Nothing)
  where go acc = do s <- get
                    case s of
                      [] -> return acc
                      (x:xs) -> case acc of
                                  Nothing                -> put xs >> go (Just [x])
                                  Just ys@((M_HYPH,_):_) -> put xs >> go (Just (x:ys))
                                  Just ys                ->
                                    case x of
                                      (M_HYPH,_) -> put xs >> go (Just (x:ys))
                                      _          -> return acc 

formatPrint :: PennTreeGen ChunkTag (POSTag,Text) -> IO ()
formatPrint x = putStrLn $ printf "%30s : %s " ((T.intercalate " " . map (^._2) . toList) x) (show x)


process basedir pp = do
  dtr <- build basedir
  let fps = take 20$  sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
  withFile "error.log" WriteMode $ \h_err -> do
    flip traverse_ parsefiles $ \f -> do
      {- 
      putStrLn "\n\n\n=============================================================================================="
      print f
      putStrLn "==============================================================================================" -}
      etr <- parseOntoNotesPennTree f
      case etr of
        Left err -> hPutStrLn h_err f
        Right trs -> 
          flip mapM_ trs $ \tr -> do
            let atr = (getADTPennTree . convertTop) tr
                nmls = filterNML atr
            when ((not.null) nmls) $ do
              mapM_ formatPrint nmls
              let terms = (filter (\(t,_) -> t /= D_NONE) . toList) atr
                  merged = evalState (unfoldM mergeHyphen) terms
              print $ map (T.concat . map snd) merged


          --  ((\xs -> when ((not.null) xs) (mapM_ formatPrint xs)) .  .  trs
                    
          {- 
          let bname = takeBaseName f
          withFile (bname <.> "corenlp_lemma") WriteMode $ \h_lemma -> do
            errorHandler h_err f (serializeLemma pp trs h_lemma)
          withFile (bname <.> "corenlp_udep") WriteMode $ \h_ud -> 
            withFile (bname <.> "corenlp_ptree") WriteMode $ \h_tr -> 
              errorHandler h_err f (serializePennTreeDep pp trs (h_ud,h_tr))
          -}

main :: IO ()
main = do
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = def & ( tokenizer .~ True )
                     . ( words2sentences .~ True )
                     . ( postagger .~ True )
                     . ( lemma .~ True )
                     . ( sutime .~ False )
                     . ( depparse .~ False ) -- . ( depparse .~ True )
                     . ( constituency .~ False ) -- . ( constituency .~ True )
                     . ( ner .~ False )
    pp <- prepare pcfg
    process basedir pp 
