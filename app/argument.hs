{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text.IO               as T.IO
import           Data.Traversable
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Prop
import           PropBank.Util                     (merge)


{- 
lookupRoleset :: PredicateDB -> (Text,Text) -> Maybe Text
lookupRoleset db (lma,sens) = do
  p <- HM.lookup lma (db^.predicateDB)
  let rs = p ^.predicate_roleset
  defn <- Data.List.lookup (lma <> "." <> sens) $ map (\r -> (r^.roleset_id,fromMaybe "" (r^.roleset_name))) rs
  return defn



  
formatStat :: PredicateDB -> ((Text,Text),Int) -> String
formatStat db ((lma,sens),num) =
  let mdefn = lookupRoleset db (lma,sens)
  in printf "%20s.%s : %5d : %s" lma sens num (fromMaybe "" mdefn)
  -}


{-  
main' = do
  let framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  
  (preddb,props) <- prepare framedir basedir
  
  lst <- flip traverse props $ \fp -> do
    hPutStrLn stderr fp
    txt <- T.IO.readFile fp
    return (parsePropWithFileField NoOmit txt)

  let rolesets = map (^.inst_lemma_roleset_id) $ concat lst
      acc = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty rolesets
  

  mapM_ (putStrLn . formatStat preddb) . sortBy (flip compare `on` snd) . HM.toList $ acc -- rolesets
-}



prepare framedir basedir = do
  propdb <- constructFrameDB framedir
  let preddb = constructPredicateDB propdb
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
  return (preddb,props)


process ptreedir framedir basedir article = do
  (preddb,props) <- prepare framedir basedir
  -- mapM_ print (take 10 props)
  flip traverse_ (find (\f -> takeBaseName f == article) props) $ \fp -> do
    hPutStrLn stderr fp
    txt <- T.IO.readFile fp
    let props = parsePropWithFileField NoOmit txt
    mapM_ print props

    let ptreefile = article <.> "corenlp_ptree"
    lbstr <- BL.readFile (ptreedir </> ptreefile)
    let mptrs = decode lbstr :: Maybe [PennTree]

    case mptrs of
      Nothing -> print "nothing"
      Just ptrs -> do
        print $ merge (^.inst_tree_id) ptrs props
                       
  --    traverse_ (mapM_ (T.IO.putStrLn . prettyPrint 0)) mptr


main = do
  let article = "wsj_2445"
      ptreedir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_20170702"
      framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
  process ptreedir framedir basedir article 
