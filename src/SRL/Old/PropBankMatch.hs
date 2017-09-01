{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Old.PropBankMatch where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Either      (EitherT,hoistEither)
import qualified Data.Attoparsec.Text       as A
import           Data.Foldable                   (toList)
import           Data.Text                       (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Time.Calendar              (fromGregorian)
--
import qualified CoreNLP.Simple.Type        as S
import           Data.Bitree                     (getLeaves)
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII      (prettyPrint)
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
--
import           SRL.Old.Type


propbank :: (FilePath,FilePath,IsOmit) -> EitherT String IO ([PennTree],[Instance])
propbank (pennfile,propfile,omit) =  do
  txt <- liftIO $ T.IO.readFile pennfile
  trs <- hoistEither $ A.parseOnly (many (A.skipSpace *> penntree)) txt
  props <- liftIO $ parseProp omit <$> T.IO.readFile propfile
  return (trs,props)


mkDocFromPennTree :: PennTree -> S.Document
mkDocFromPennTree = flip S.Document (fromGregorian 2017 4 17)
                  . T.intercalate " "
                  . map snd
                  . filter (\(t :: Text,_) -> t /= "-NONE-")
                  . getLeaves  


showMatchedInstance :: (Int,SentenceInfo,PennTree,[Instance]) -> IO ()
showMatchedInstance (_i,sentinfo,tr,_prs) = do
  let pt = sentinfo^.corenlp_tree
      terms = map (^._2) . toList $ pt
  T.IO.putStrLn "================="
  T.IO.putStrLn "PropBank"
  T.IO.putStrLn $ prettyPrint 0 tr
  T.IO.putStrLn "-----------------"
  T.IO.putStrLn "CoreNLP"  
  T.IO.putStrLn $ prettyPrint 0 pt
  T.IO.putStrLn "-----------------"            
  T.IO.putStrLn (T.intercalate " " terms)
  T.IO.putStrLn "-----------------"
  -- mapM_ printMatchedInst $ matchInstances (pt,tr) prs



