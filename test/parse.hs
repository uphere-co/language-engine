{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens       hiding (element,elements)
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Taggy.Lens
--
import           PropBank.Parser


main = do
  putStrLn "parse propbank xml file"
  print =<< parseFile "/home/wavewave/repo/srcc/propbank-frames/frames/delouse.xml"
{- 
  -- txt <- TLIO.readFile "/home/wavewave/repo/workspace/nombank/nombank.1.0/frames/investment.xml"
  txt <- TLIO.readFile "/home/wavewave/repo/srcc/propbank-frames/frames/delouse.xml"
  
  let frameset =  head (txt ^.. html . allNamed (only "frameset"))
      -- predicates = frameset ^.. allNamed (only "predicate")
  (print . p_frameset) frameset
-}
  
