{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens hiding (para)
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
import qualified Data.Text.Lazy.IO   as T.L.IO
import           Data.Text.Read                (decimal)
import           System.Directory
import           System.FilePath
-- import           Text.PrettyPrint hiding ((<>)) --              (hcat,nest,render,text,vcat,($$))
import           Text.PrettyPrint.Boxes hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           OntoNotes.Parser.SenseInventory


load dir = do
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts) 
  -- let fs = ["stock-v.xml"] -- [ "fracture-v.xml" ] -- [ "get-v.xml" ]
  flip traverse fs $ \f -> do
    let fp = dir  </> f
    -- print fp
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just f -> case p_inventory f of
                  Left err -> error err
                  Right c  -> return c


getSenses lma simap = do
  si <- maybeToList (HM.lookup lma simap)
  s <- si^.inventory_senses
  let txt1 = text (printf "%2s.%-6s %-40s   " (s^.sense_group) (s^.sense_n) (T.take 40 (s^.sense_name)))
      txt_fn = vcat top (map (text.show) (catMaybes (s^..sense_mappings.mappings_fn)))
      txt_wn = vcat top $ let lst = map (text.printf "%-30s") (catMaybes (s^..sense_mappings.mappings_wn.traverse.wn_lemma))
                          in if null lst then [text (printf "%-30s" ("" :: String))] else lst
                                              
  return (txt1 <+> txt_wn <+> txt_fn)
  -- return (s^.sense_group,s^.sense_n , s^.sense_name, s^.sense_mappings)


main :: IO ()
main = do
  let dir= "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
  sis <- load dir
  let simap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  txt <- T.IO.readFile "run/OntoNotes_propbank_statistics_only_wall_street_journal.txt"
  let ws = map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)

      merge :: [(Text,Text)] -> (Text,Int)
      merge lst = let (lma,_) = head lst
                      -- (lma,_) = T.break (== '.') l
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))
                     
      merged = sortBy (flip compare `on` snd) . map merge . groupBy ((==) `on` fst)
             . sortBy (flip compare `on` fst)
             . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
             $ ws 
   
  forM_ merged $ \(lma,f) -> do
    let lmav = lma <> "-v"
        doc = text (printf "%20s : %6d " lma f) <+> vcat top (getSenses lmav simap)
    putStrLn "-------------------------------------------"
    putStrLn (render doc)
 
