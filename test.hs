{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Array                (array)
import           Data.Graph
import qualified Data.IntMap as IM
import           Data.List                 (sort)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Tuple                (swap)
import           System.Directory
import           System.FilePath
--
import           SRL.CoNLL.CoNLL08.Parser


showSent :: Sentence -> Text
showSent s = T.intercalate " " (s^.sentence_tokens) 


findChildren :: Graph -> IM.IntMap Text -> Int -> [Text]
findChildren gr m = fromMaybe [] . mapM (\k -> IM.lookup k m) . sort . reachable (transposeG gr)


format :: Sentence -> Text
format s = let gr = depGraph s
               m = IM.fromList (zip [1..] (s^.sentence_tokens))
               formatArg (a,n) = a <> " : " <> case IM.lookup n m of {Nothing -> "" ; Just w -> w} 
                                   <> " : " <> T.intercalate " " (findChildren gr m n)
               f ((i0,rset),args) = rset <> "\n" <> "-----\n" <> 
                                    T.intercalate "\n" (map formatArg args) <>
                                    "\n------"
           in T.intercalate "\n" (map f (s^.sentence_preds)) 

display :: Sentence -> IO ()
display s = do
  putStrLn "============"
  TIO.putStrLn (showSent s)
  putStrLn "------------"
  -- print (s^.sentence_deps)
  putStrLn "------------"
  TIO.putStrLn (format s)


depGraph :: Sentence -> Graph
depGraph s = let n = length (s ^. sentence_lines)
             in buildG (0,n) (s^.sentence_deps)

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
  
