{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                     (forM_)
import qualified Data.Attoparsec.Text         as A
import           Data.Char
import           Data.Function                     (on)
import           Data.List                         (groupBy,sortBy)
import           Data.List.Split                   (splitWhen)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.FilePath                   ((</>))
--
import           NLP.Parser.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util
  
propbank :: IO ()
propbank =  do
  let omit = NoOmit
  props <- parseProp NoOmit <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/chZ.mrg"
  let xs = T.lines txt
      txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
  case mapM (A.parseOnly penntree) txts of
    Left err -> print err
    Right trs -> do
      mapM_ showSentenceProp (merge (^.inst_tree_id) trs props) -- lst

 
nombank :: IO ()
nombank =  do
  let treedir = "/scratch/wavewave/Penn-tbank/MRG"
  nomprops <- parseNomProp <$> TIO.readFile "/scratch/wavewave/nombank.1.0/nombank.1.0"

  let nomprop_grps = groupBy ((==) `on` (^.nominst_tree_file))
                   . sortBy (compare `on` (^.nominst_tree_file))
                   $ nomprops
  print $ length (nomprop_grps)
  forM_ (take 2 nomprop_grps) $ \xs@(p:ps) -> do
    let fp = treedir </> map toUpper (p^.nominst_tree_file)
    txt <- TIO.readFile fp
    case (A.parseOnly (many (A.skipSpace *> penntree)) txt) of
      Left err -> error err
      Right trs ->
        mapM_ showSentenceNom $ merge (^.nominst_tree_id) trs xs


main = nombank
