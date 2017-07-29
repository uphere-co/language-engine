{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Lens
import           Data.Foldable
import           Data.Function                (on)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.List
import           Data.Maybe                   (fromMaybe,maybeToList,mapMaybe)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Traversable
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Prop


lookupRoleset :: PredicateDB -> (Text,Text) -> Maybe Text
lookupRoleset db (lma,sens) = do
  p <- HM.lookup lma (db^.predicateDB)
  let rs = p ^.predicate_roleset
  defn <- Data.List.lookup (lma <> "." <> sens) $ map (\r -> (r^.roleset_id,fromMaybe "" (r^.roleset_name))) rs
  return defn

maybeNumberedArgument :: PropBankLabel -> Maybe Int
maybeNumberedArgument (NumberedArgument n) = Just n
maybeNumberedArgument _                    = Nothing

formatInst :: Instance -> String
formatInst inst = show (mapMaybe maybeNumberedArgument (inst^..inst_arguments.traverse.arg_label))



formatStatInst :: PredicateDB -> HashMap RoleSetID [Instance] -> (RoleSetID,Int) -> String
formatStatInst db imap (rid,num) =
  let mdefn = lookupRoleset db rid
      minsts = HM.lookup rid imap
  in printf "%20s : %5d : %s\n" (formatRoleSetID rid) num  (fromMaybe "" mdefn)
     ++ (intercalate "\n" . map formatInst . concat . maybeToList) minsts




main = do
  let propframedir = "/home/wavewave/repo/srcc/propbank-frames/frames" -- "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
  propdb <- constructFrameDB propframedir
  let preddb = constructPredicateDB propdb


  let basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"

  dtr <- build basedir

  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
  instss <- flip traverse props $ \fp -> do
    hPutStrLn stderr fp
    txt <- TIO.readFile fp
    return (parsePropWithFileField NoOmit txt)

  let all_insts_v = filter (\p->T.last (p^.inst_lemma_type) == 'v') (concat instss)
      insts_v = filter (\x -> x^.inst_lemma_roleset_id._1 == "call") all_insts_v

      classified_insts = foldl' addfunc  HM.empty insts_v
        where addfunc acc inst = HM.insertWith (++) (inst^.inst_lemma_roleset_id) [inst] acc
      rolesets = map (^.inst_lemma_roleset_id) insts_v
      stat = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty rolesets

  mapM_ (putStrLn . formatStatInst preddb classified_insts) . sortBy (flip compare `on` snd) . HM.toList $ stat


  -- mapM_ (putStrLn . formatStat preddb) . sortBy (flip compare `on` snd) . HM.toList $ acc
