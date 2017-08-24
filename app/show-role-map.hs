{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens              ((^.),_1,_2)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (intercalate)
import           Data.Text                 (Text)
import qualified Data.Text           as T
import           Text.Printf               (printf)
--
import           Lexicon.Format            (formatArgPatt,formatRoleMap)
import           Lexicon.Query             (loadRolePattInsts)
import           Lexicon.Type              (ArgPattern(..))
import           NLP.Type.SyntaxProperty   (Voice)


main = do
  let verb_subcat_file = "/scratch/wavewave/run/20170820/verbsubcat_propbank_ontonotes_statonly.tsv" 
  subcats <- loadRolePattInsts verb_subcat_file
  -- rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  
  -- let flattened = createONFN subcats sensemap framedb rolesetdb
  --     indexed = zip [1..] flattened

  
  flip mapM_ subcats $ \subcat -> do
    let argpattstr = intercalate "\n" $ flip map (Prelude.take 10 (subcat^._2)) $
                       \(patt :: ArgPattern Voice Text,n) ->
                         printf "%s     #count: %5d" (formatArgPatt "voice" patt) (n :: Int)
    print (subcat^._1)
    putStrLn argpattstr
  
