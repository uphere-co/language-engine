{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens hiding (para)
import           Data.Foldable
import           Data.Function
import           Data.HashMap.Strict           (HashMap)
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
import           Text.PrettyPrint.Boxes hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           OntoNotes.Parser.SenseInventory


data VorN = V | N deriving Eq


data Config = Config { _cfg_sense_inventory_file :: FilePath
                     , _cfg_semlink_file         :: FilePath
                     , _cfg_statistics           :: FilePath
                     }

makeLenses ''Config
              
cfg = Config { _cfg_sense_inventory_file = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
             , _cfg_semlink_file = "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
             , _cfg_statistics = "run/OntoNotes_propbank_statistics_only_wall_street_journal.txt"               
             }


loadSenseInventory dir = do
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts)
  flip traverse fs $ \f -> do
    let fp = dir  </> f
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just f -> case p_inventory f of
                  Left err -> error err
                  Right c  -> return c



createVNFNDB :: VNFNMappingData -> HashMap (Text,Text) [Text]
createVNFNDB semlink = 
  let lst = map (\c-> ((c^.vnc_vnmember,c^.vnc_class),c^.vnc_fnframe)) (semlink^.vnfnmap_vnclslst)
  in foldl' (\(!acc) (k,v) -> HM.insertWith (++) k [v] acc) HM.empty lst


loadSemLink file = do
  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnmappingdata f of
                Left err -> error err
                Right c -> return c
  
loadStatistics file = do
  txt <- T.IO.readFile file
  return $ map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)


verbnet semlinkmap lma txt =
  let (_,cls') = T.breakOn "-" txt
  in if T.null cls'
     then text (printf "%-43s" ("" :: String))
     else let cls = T.tail cls'
              frms = fromMaybe [] (HM.lookup (lma,cls) semlinkmap)
          in text (printf "%-8s -> " cls) <+>
             vcat top (if null frms
                       then [text (printf "%-30s" ("":: String))]
                       else map (text.printf "%-30s") frms
                      )




getSenses lma vorn sensemap semlinkmap = do
  let lmav = lma <> case vorn of V -> "-v" ; N -> "-n"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let txt1 = text (printf "%2s.%-6s %-40s   " (s^.sense_group) (s^.sense_n) (T.take 40 (s^.sense_name)))
      mappings = s^.sense_mappings
      txt_pb = vcat top $ let lst = T.splitOn "," (mappings^.mappings_pb)
                          in if null lst
                             then [text (printf "%-20s" ("" :: String))]
                             else map (text.printf "%-20s") lst
      txt_fn = vcat top $ let lst = maybe [] (T.splitOn ",") (mappings^.mappings_fn)
                          in if null lst
                             then [text (printf "%-30s" ("" :: String))]
                             else map (text.printf "%-30s") lst
      txt_wn = vcat top $ let lst = map (text.printf "%-30s") (catMaybes (mappings^..mappings_wn.traverse.wn_lemma))
                          in if null lst then [text (printf "%-30s" ("" :: String))] else lst
      txt_vn = case vorn of
                 V -> vcat top $ let lst = maybe [] (map (verbnet semlinkmap lma) . T.splitOn ",") (mappings^.mappings_vn)
                                 in if null lst
                                    then [text (printf "%-43s" ("" :: String))]
                                    else lst --  map (text.printf "%-20s") lst
                 N -> vcat top [text (printf "%-42s" ("" :: String))]


  return (txt1 <+> txt_pb <+> txt_fn <+> txt_vn <+> txt_wn )


main :: IO ()
main = do
  semlink <- loadSemLink (cfg^.cfg_semlink_file)
  let semlinkmap = createVNFNDB semlink

  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)

  let merge :: [(Text,Text)] -> (Text,Int)
      merge lst = let (lma,_) = head lst
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))

      merged = sortBy (flip compare `on` snd) . map merge . groupBy ((==) `on` fst)
             . sortBy (flip compare `on` fst)
             . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
             $ ws

  forM_ merged $ \(lma,f) -> do
    let -- lmav = lma <> "-v"
        doc = text (printf "%20s:%6d " lma f) <+> vcat top (getSenses lma V sensemap semlinkmap )
    putStrLn "-------------------------------------------"
    putStrLn (render doc)

