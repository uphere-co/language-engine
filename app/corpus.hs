{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as A
import           Data.Foldable               (toList)
import           Data.List.Split             (splitWhen)
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           NLP.Parser.PennTreebankII
--
import           PropBank.Parser.Prop



                 
main0 :: IO ()
main0 = do
  putStrLn "corpus"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  mapM_ print $ take 2 (parseProp txt)


main :: IO ()
main =  do
  main0 
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/chZ.mrg"
  let xs = T.lines txt
      txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
      testtxt = head txts
  -- A.parseOnly penntree
  -- TIO.putStrLn txt
  TIO.putStrLn testtxt
  case A.parseOnly penntree testtxt of
    Left err -> print err
    Right tr -> do -- print $ zip [0..] $ toList tr
      let mn = findNode (2,2) tr
      case mn of
        Nothing -> error "your index was wrong"
        Just n -> (TIO.putStrLn . T.intercalate " " . map snd . toList) n
