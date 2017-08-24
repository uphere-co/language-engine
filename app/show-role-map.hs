{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens              ((^.),_1,_2)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (find,intercalate)
import           Data.Text                 (Text)
import qualified Data.Text           as T
import           Text.Printf               (printf)
--
import           Lexicon.Format            (formatArgPatt,formatRoleMap)
import           Lexicon.Query             (loadRoleInsts,loadRolePattInsts)
import           Lexicon.Type              (ArgPattern(..))
import           NLP.Type.SyntaxProperty   (Voice)


main = do
  let verb_subcat_file = "/scratch/wavewave/run/20170820/verbsubcat_propbank_ontonotes_statonly.tsv"
      rolemap_file = "/home/wavewave/repo/srcp/OntoNotes/mapping/final.txt" 
  subcats <- loadRolePattInsts verb_subcat_file
  rolemap <- loadRoleInsts rolemap_file
  
  
  flip mapM_ (drop 100 (take 150 subcats)) $ \subcat -> do
    let argpattstr = intercalate "\n" $ flip map (subcat^._2) $
                       \(patt :: ArgPattern Voice Text,n) ->
                         printf "%s     #count: %5d" (formatArgPatt "voice" patt) (n :: Int)
    print (subcat^._1)
    putStrLn "\n"
    case find (\rm -> (rm^._1) == (subcat^._1)) rolemap of
      Nothing -> return ()
      Just rm -> do
        putStrLn $ formatRoleMap  (rm^._2)
        putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    putStrLn argpattstr
    putStrLn "\n\n\n\n"  
