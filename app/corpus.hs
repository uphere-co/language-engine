{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens
import qualified Data.Attoparsec.Text as A
import           Data.Discrimination         (leftOuter)
import           Data.Discrimination.Grouping (grouping)
import           Data.Foldable               (toList)
import           Data.Function               (on)
import qualified Data.IntMap          as IM
import           Data.List                   (groupBy)
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
import           PropBank.Type.Prop

display1 :: (Int,(PennTree,[Instance])) -> IO ()
display1 (i,(tr,props)) = do
  TIO.putStrLn "================================================="
  TIO.putStr ("Sentence " <> T.pack (show i) <> ": ") 
  (TIO.putStrLn . T.intercalate " " . toList) tr
  mapM_ (display . (tr,)) props


main :: IO ()
main =  do
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

      mapM_ display1 lst
