{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
import           Data.Either                      (rights)
import           Data.Foldable
import           Data.Function
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Text.Read                   (decimal)
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common (fr_frame)
import           FrameNet.Type.LexUnit
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS
--
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.App.Load


data VorN = V | N 

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

framesFromLU :: LexUnitDB -> Text -> [Text]
framesFromLU ludb lma = do
  i <- fromMaybe [] (HM.lookup lma (ludb^.nameIDmap))
  l <- maybeToList (IM.lookup i (ludb^.lexunitDB))
  frm <- maybeToList (l^.lexunit_frameReference^.fr_frame)
  return frm
  


formatSenses lma vorn sensemap semlinkmap sensestat = do
  let lmav = lma <> case vorn of V -> "-v" ; N -> "-n"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt1 = text (printf "%2s.%-6s (%4d cases) |  " (s^.sense_group) (s^.sense_n) num )
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
      txt_definition = text ("definition: " ++ T.unpack (s^.sense_name))
      txt_commentary = text (T.unpack (fromMaybe "" (s^.sense_commentary)))
      txt_examples   = text (T.unpack (s^.sense_examples))
      txt_detail = vcat left [txt_definition,txt_commentary,txt_examples]
  return (vcat left [(txt1 <+> txt_pb <+> txt_fn <+> txt_vn <+> txt_wn),txt_detail])




listWordNets :: WN -> [Text]
listWordNets wn = T.splitOn "," (wn^.wn_contents)


formatWordNet lma vorn sensemap sensestat wndb = do
  let lmav = lma <> case vorn of V -> "-v" ; N -> "-n"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt1 = text (printf "%2s.%-6s (%4d cases) |  " (s^.sense_group) (s^.sense_n) num )
      mappings = s^.sense_mappings
      wns = mappings^..mappings_wn.traverse
      txt_wn = vcat top $ do wn <- wns
                             snumtxt <- listWordNets wn
                             snum <- fst <$> rights [decimal snumtxt]
                             let l = case wn^.wn_lemma of
                                       Nothing -> lma
                                       Just l' -> l'
                                 wnsense = l <> "." <> T.pack (show snum)
                                 result = find (\x -> x^._1 == SenseNumber snum) (lookupLemma wndb POS_V l)
                                 restxt = flip (maybe "") result $ \(_,xs,txt) ->
                                            printf "%-20s: %-45s | %s"
                                              wnsense
                                              (T.intercalate "," (map formatLI xs))
                                              txt
                                 --  formatSynset (xs,txt)) result
                                 -- SenseNumber snum
                                 
                             return (text restxt)

      txt_definition = map (text . T.unpack . T.strip) $ T.lines (s^.sense_name)
      txt_commentary = map (text . T.unpack . T.strip) $ T.lines (fromMaybe "" (s^.sense_commentary))
      txt_examples   = map (text . T.unpack . T.strip) $ T.lines (s^.sense_examples)
      txt_detail = vcat left (txt_definition ++ txt_commentary ++ txt_examples)
  return $ (txt1 <+> vcat left [txt_detail,txt_wn]) //
           text "---------------------------------------------------------------------------------------------------------------"



formatStat :: ((Text,Text),Int) -> String
formatStat ((lma,sens),num) = printf "%20s.%-5s : %5d" lma sens num


listSenseDetail :: IO ()
listSenseDetail = do
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAll
  
  let merged = mergeStatPB2Lemma ws 
  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma    
    let frms = framesFromLU ludb (lma <> ".v")
        doc = text (printf "%20s:%6d " lma f) //
              vcat top (formatSenses lma V sensemap semlinkmap sensestat )
{- 
    let findnonzero = do
          si <- maybeToList (HM.lookup (lma <> "-v") sensemap)
          s <- si^.inventory_senses
          let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
          guard (num > 0)
          return (s^.sense_group, s^.sense_n, num)

              
    T.IO.putStrLn lma
    mapM_ (\(g,n,num)->putStrLn (printf "%1s.%-6s" g n)) findnonzero
    putStrLn ""
-}
    putStrLn "====================================================================================================================="
    putStrLn $ "From FrameNet Lexical Unit " ++ show (lma <> ".v") ++ ": " ++ show frms
    putStrLn (render doc)


mergeStatPB2Lemma ws = 
  let merge :: [(Text,Text)] -> (Text,Int)
      merge lst = let (lma,_) = head lst
                  in case mapM (decimal.snd) lst of
                       Left _     -> (lma,0)
                       Right lst' -> (lma,sum (map fst lst'))

  in sortBy (flip compare `on` snd) . map merge . groupBy ((==) `on` fst)
     . sortBy (flip compare `on` fst)
     . map (\(l,f)-> let (lma,_) = T.break (== '.') l in (lma,f))
     $ ws



listSenseWordNet :: IO ()
listSenseWordNet = do
  (ludb,sensestat,semlinkmap,sensemap,ws,wndb) <- loadAll
  
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)
  let merged = mergeStatPB2Lemma ws

  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma    
    let doc = text "=====================================================================================================================" //
              text (printf "%20s:%6d " lma f) //
              text "---------------------------------------------------------------------------------------------------------------" // 
              vcat top (formatWordNet lma V sensemap sensestat wndb)
    putStrLn (render doc)
  


main = listSenseWordNet
