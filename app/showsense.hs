{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.Text         as A
import           Data.Discrimination                  (joining)
import           Data.Discrimination.Grouping
import           Data.Either.Extra
import           Data.Foldable                        (toList,traverse_)
import           Data.List                            (lookup,sort)
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import           System.Directory.Tree
import           System.FilePath
import           System.IO
--
import           Data.Attribute
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           OntoNotes.Parser.Sense

parseSenseFile :: FilePath -> IO (Either String [SenseInstance])
parseSenseFile fp = do
  txt <- T.IO.readFile fp
  let lst = T.lines txt
      wss = map T.words lst
  return (traverse parseSenseInst wss)
  --   mapM_ (print . parseSenseInst) wss


elookup str k = hoistEither.maybeToEither str.lookup k

formatSense sentterms inst = do
  let n = inst^.sinst_sentence_id
      k = inst^.sinst_token_id
  terms <- elookup ("no such sentence: " ++ show n) n sentterms
  token <- elookup ("no such token: " ++ show k) k terms
  liftIO $ print token
  liftIO $ print inst
  

main :: IO ()
main = do
  putStrLn "OntoNotes: section Wall Street Journal"
  putStrLn "======================================"
  
  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/00"

  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
      sensefiles = filter (\x -> takeExtensions x == ".sense") fps
      propfiles  = filter (\x -> takeExtensions x == ".prop" ) fps 
      joinf = joinAttrib takeBaseName
      lst :: [(String,Maybe FilePath,Maybe FilePath,FilePath)]
      lst = map toTuple (propfiles `joinf` (sensefiles `joinf` map (\x -> takeBaseName x `acons` x `acons` anil) parsefiles))
  -- mapM_ print lst
  {-     joined = joining grouping headMatch takeBaseName takeBaseName sensefiles parsefiles
        where headMatch (x:_) (y:_) = Just (x,y)
              headMatch _     _     = Nothing
  -}

  
  flip traverse_ lst $ \(_,mfp_prop,mfp_sense,fp_parse) -> do
    case (mfp_prop,mfp_sense) of
      (Just fp_prop,Just fp_sense) -> do
        r <- runEitherT $ do
          liftIO $ putStrLn (fp_sense)
          txt_parse <- liftIO (T.IO.readFile fp_parse)
          trs <- hoistEither (A.parseOnly (A.many1 (A.skipSpace *> pnode)) txt_parse)
          let sentterms = zip [0..] $
                            map (zip [0..] . map snd . filter (\(t,_) -> t /= "-NONE-") . toList) trs
          -- traverse_ (T.IO.putStrLn . prettyPrint 0) trs
          traverse_ (liftIO . print) sentterms
          insts <- EitherT $ parseSenseFile fp_sense
          traverse_ (formatSense sentterms) insts

        case r of
          Left err -> error err
          Right () -> return ()
      _ -> return ()  
