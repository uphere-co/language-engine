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
import           Data.List                            (group,intercalate,sort)
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
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Match
--
import           OntoNotes.App.Serializer

getTerms :: PennTree -> [Text]
getTerms = map snd . filter (\(t,_) -> t /= "-NONE-") . toList


getNONETerms :: PennTree -> [Text]
getNONETerms = map snd . filter (\(t,_) -> t == "-NONE-") . toList


parseOntoNotesPennTree :: FilePath -> IO (Either String [PennTree])
parseOntoNotesPennTree f = fmap (A.parseOnly (A.many1 (A.skipSpace *> pnode))) (T.IO.readFile f)


errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()


filterNML x@(PN NML xs) = [x]
filterNML (PN c xs) = concatMap filterNML xs
filterNML (PL _) = []



process basedir pp = do
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
          let txts = flip map trs $ \tr ->
                let merged = getMerged tr
                in T.intercalate " " (map (T.concat . map (snd.snd)) merged)
          let bname = takeBaseName f
          withFile (bname <.> "corenlp_lemma") WriteMode $ \h_lemma -> do
            errorHandler h_err f $ do
              anns <- annotateTexts pp txts
              rdocs' <- traverse protobufDoc anns
              let rdocs = sequenceA rdocs'
              mbstr <- serializeLemma rdocs
              case mbstr of
                Just bstr -> BL.hPutStrLn h_lemma bstr
                Nothing -> return ()
          withFile (bname <.> "corenlp_udep") WriteMode $ \h_ud ->
            withFile (bname <.> "corenlp_ptree") WriteMode $ \h_tr ->
              errorHandler h_err f $ do 
                anns <- annotateTexts pp txts
                rdocs' <- traverse protobufDoc anns
                let rdocs = sequenceA rdocs'
                mbstr <- serializePennTreeDep rdocs
                case mbstr of
                  Just (bstr_deps,bstr_ntrs) -> do
                    BL.hPutStrLn h_ud bstr_deps
                    BL.hPutStrLn h_tr bstr_ntrs
                  Nothing -> return ()


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
                     . ( depparse .~ True )
                     . ( constituency .~ True )
                     . ( ner .~ False )
    pp <- prepare pcfg
    process basedir pp
