{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                     (forM_)
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
import           NLP.Type.PennTreebankII
--
import           PropBank.Parser.Prop
import           PropBank.Type.Prop

merge :: (b -> Int) -> [a] -> [b] -> [(Int,(a,[b]))] 
merge idxf trs props =
   let itrs = (zip [0..] trs)
       propgrps = (map (\x->(idxf (head x),x)) . groupBy ((==) `on` idxf)) props
   in concat $ leftOuter grouping joiner m1 fst fst itrs propgrps
 where joiner (i,x) (_,y) = (i,(x,y))
       m1 (i,x) = (i,(x,[]))
  
main :: IO ()
main =  do
  props <- parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Penn_Treebank-orig/data/written/chZ.mrg"
  let xs = T.lines txt
      txts = map T.unlines . filter (not.null) $ splitWhen T.null xs
  case mapM (A.parseOnly penntree) txts of
    Left err -> print err
    Right trs -> do
      mapM_ showSentenceAnnotation (merge (^.inst_tree_id) trs props) -- lst

{- 
main :: IO ()
main =  do
  -- props <- parseProp <$> TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  let treedir = "/scratch/wavewave/Penn-tbank/MRG"
  nomprops <- parseNomProp <$> TIO.readFile "/scratch/wavewave/nombank.1.0/nombank.1.0"

  let nomprop_grps = groupBy ((==) `on` (^.nominst_tree_file)) nomprops
  print $ length (nomprop_grps)
  forM_ (take 1 nomprop_grps) $ \xs@(p:ps) -> do
    let fp = treedir </> map toUpper (p^.nominst_tree_file)
    txt <- TIO.readFile fp
    -- TIO.putStrLn txt
    case (A.parseOnly (many (A.skipSpace *> penntree)) txt) of
      Left err -> error err
      Right trs ->
        -- mapM_ print trs
        -- print (groupBy ((==) `on` (^.inst_tree 

-}
  {- 
  let p = head nomprops

  let fp = treedir </> map toUpper (p^.nominst_tree_file)
  txt <- TIO.readFile fp
  TIO.putStrLn txt
  case (A.parseOnly (many (A.skipSpace *> penntree)) txt) of
    Left err -> error err
    Right trs ->
      zmapM_ print trs
  -}
