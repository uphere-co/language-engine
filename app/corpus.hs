{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.Attoparsec.Text         as A
import           Data.Char
import           Data.Discrimination               (leftOuter)
import           Data.Discrimination.Grouping      (grouping)
import           Data.Function                     (on)
import           Data.List                         (groupBy)
import           Data.List.Split                   (splitWhen)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.FilePath                   ((</>))
--
import           NLP.Parser.PennTreebankII
--
import           PropBank.Parser.Prop
import           PropBank.Type.Prop

mainProp :: IO ()
mainProp =  do
  props <- parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/chZ.mrg"
  let xs = T.lines txt
      txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
  case mapM (A.parseOnly penntree) txts of
    Left err -> print err
    Right trs -> do
      let itrs = (zip [0..] trs)
          propgrps = (map (\x->((head x)^.inst_tree_id,x)) . groupBy ((==) `on` (^.inst_tree_id))) props
          lst = concat $ leftOuter grouping joiner m1 fst fst itrs propgrps
            where joiner (i,x) (_,y) = (i,(x,y))
                  m1 (i,x) = (i,(x,[]))

      mapM_ showSentenceAnnotation lst

main :: IO ()
main =  do
  -- props <- parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  let treedir = "/scratch/wavewave/Penn-tbank/MRG"
  nomprops <- parseNomProp <$> TIO.readFile "/scratch/wavewave/nombank.1.0/nombank.1.0"

  let p = head nomprops

  let fp = treedir </> map toUpper (p^.nominst_tree_file)

  print fp
  print p
