module Main where

import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import           PropBank.Parser.Prop



                 
main :: IO ()
main = do
  putStrLn "propcorpus"
  txt <- TIO.readFile "/scratch/wavewave/MASC/Propbank/Propbank-orig/data/written/chZ.prop"
  mapM_ print $ take 2 (parseProp txt)
  
  -- TIO.putStrLn txt
  
