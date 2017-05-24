{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.Attoparsec.Text as A
import           Data.Foldable               (toList)
import           Data.List.Split             (splitWhen)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           NLP.Parser.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           PropBank.Parser.Prop

display :: (PennTree,Instance) -> IO ()
display (tr,prop) = do
  (TIO.putStrLn . T.intercalate " " . toList) tr
  print prop
  let args = prop^.inst_arguments
  mapM_ (displayArg tr) args
  
displayArg :: PennTree -> Argument -> IO ()
displayArg tr arg = do
  TIO.putStrLn ("-----" <> arg^.arg_label <> "-----")
  let format n = (T.intercalate " " . map snd . toList) n
  mapM_ (\x -> TIO.putStrLn (maybe "Nothing" format (findNode x tr))) (arg^.arg_terminals)


main :: IO ()
main =  do
  props <- parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/chZ.mrg"
  let xs = T.lines txt
      txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
      -- testtxt = htxts
  -- A.parseOnly penntree
  -- TIO.putStrLn txt
  -- TIO.putStrLn testtxt
  case mapM (A.parseOnly penntree) txts of
    Left err -> print err
    Right trs -> do
      let pennprops = zip trs props  
      flip mapM_ pennprops $ \x@(tr,prop) -> do
        display x
        {- 
        let mn = findNode (Node 2 2) tr
        case mn of
          Nothing -> error "your index was wrong"
          Just n -> (TIO.putStrLn . T.intercalate " " . map snd . toList) n
        -}
