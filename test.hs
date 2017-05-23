{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.IntMap as IM
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Directory
import           System.FilePath
--
import           SRL.CoNLL.CoNLL08.Parser


showSent :: Sentence -> Text
showSent s = T.intercalate " " (s^.sentence_tokens) 

format :: Sentence -> Text
format s = let m = IM.fromList (zip [1..] (s^.sentence_tokens))
               formatArg (a,n) = a <> " : " <> case IM.lookup n m of {Nothing -> "" ; Just w -> w} 
                       
               f ((i0,rset),args) = rset <> "\n" <> "-----\n" <> 
                                    T.intercalate "\n" (map formatArg args) <>
                                    "\n------"
           in T.intercalate "\n" (map f (s^.sentence_preds))

display :: Sentence -> IO ()
display s = do
  putStrLn "============"
  TIO.putStrLn (showSent s)
  putStrLn "------------"
  TIO.putStrLn (format s)


main = do
  let dir = "/scratch/wavewave/MASC/masc-conll/data/written"
  cnts <- getDirectoryContents dir
  let fs = take 1 $ filter (\x->takeExtensions x == ".conll") cnts
  flip mapM_ fs $  \f -> do
    let fp = dir </> f
    print fp
    sents <- parseFile fp
    let ds = do s <- sents
                l <- s^.sentence_lines
                return (l^.line_deprel)
    mapM_ display (take 10 sents)
  
