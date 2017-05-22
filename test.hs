module Main where

import Control.Lens
import qualified Data.HashSet as S
import SRL.CoNLL.Parser


main = do
  sents <- parseFile "/scratch/wavewave/MASC/masc-conll/data/written/ch5.conll"
  let ds = do s <- sents
              l <- s^.sentence_lines
              return (l^.line_deprel)
  --    s = S.fromList ds  
  mapM_ print ds
  -- print $ length s 
  -- mapM_ putStrLn $ map (^..sentence_lines.folded.line_deprel.folded) sents 
  
