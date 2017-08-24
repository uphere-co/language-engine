{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens              ((^.),_1,_2)
import           Data.Function             (on)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (find,intercalate,sortBy)
import           Data.Text                 (Text)
import qualified Data.Text           as T
import           Text.Printf               (printf)
--
import           NLP.Type.SyntaxProperty   (Voice)
--
import           Lexicon.Format            (formatArgPatt,formatRoleMap)
import           Lexicon.Merge             (mergePatterns,patternGraph,patternRelation,listOfSupersetSubset,topPatterns)
import           Lexicon.Query             (loadRoleInsts,loadRolePattInsts)
import           Lexicon.Type              (ArgPattern(..))


main = do
  let verb_subcat_file = "/scratch/wavewave/run/20170820/verbsubcat_propbank_ontonotes_statonly.tsv"
      rolemap_file = "/home/wavewave/repo/srcp/OntoNotes/mapping/final.txt"
  subcats <- loadRolePattInsts verb_subcat_file
  rolemap <- loadRoleInsts rolemap_file


  flip mapM_ (drop 100 (take 150 subcats)) $ \subcat -> do
    let pattstats0 = subcat^._2
        pattstats = mergePatterns (subcat^._2)
        ipattstats = zip [1..] pattstats
        pattgraph = patternGraph (patternRelation `on` (^._1)) ipattstats
        supersub  = listOfSupersetSubset pattgraph

        toppatts = sortBy (flip compare `on` snd) (topPatterns ipattstats supersub)
        formatArgPattStat pstats =
          intercalate "\n" $ flip map pstats $ \(patt,n) ->
            printf "%s     #count: %5d" (formatArgPatt "voice" patt) (n :: Int)
        argpattstr0 = formatArgPattStat pattstats0
        argpattstr = formatArgPattStat pattstats
        topargpattstr = formatArgPattStat toppatts

    print (subcat^._1)
    putStrLn "\n"
    case find (\rm -> (rm^._1) == (subcat^._1)) rolemap of
      Nothing -> return ()
      Just rm -> do
        putStrLn $ formatRoleMap  (rm^._2)
    putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    putStrLn argpattstr0
    putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    putStrLn argpattstr
    putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    putStrLn topargpattstr

    putStrLn "\n\n\n\n"
