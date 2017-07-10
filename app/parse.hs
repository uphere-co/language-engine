{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens                  hiding ((<.>))
import           Data.Aeson
import qualified Data.Attoparsec.Text         as A
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Default
import           Data.Foldable                        (toList,traverse_)
import qualified Data.IntMap                  as IM
import           Data.List                            (sort)
import           Data.Maybe
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


process pp = do
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
  withFile "error.log" WriteMode $ \h_err -> do
    flip traverse_ parsefiles $ \f -> do
      putStrLn "\n\n\n=============================================================================================="
      print f
      putStrLn "=============================================================================================="
      etr <- parseOntoNotesPennTree f
      case etr of
        Left err -> hPutStrLn h_err f
        Right trs -> do
          let bname = takeBaseName f
          withFile (bname <.> "corenlp_lemma") WriteMode $ \h_lemma -> do
            errorHandler h_err f (serializeLemma pp trs h_lemma)
          withFile (bname <.> "corenlp_udep") WriteMode $ \h_ud -> 
            withFile (bname <.> "corenlp_ptree") WriteMode $ \h_tr -> 
              errorHandler h_err f (serializePennTreeDep pp trs (h_ud,h_tr))


main :: IO ()
main = do
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
    process pp 
