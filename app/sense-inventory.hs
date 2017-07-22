{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either.Extra                (fromRight)
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
import qualified Data.Text.Lazy.IO          as T.L.IO
import           Data.Text.Read                   (decimal)
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common (fr_frame)
import           FrameNet.Type.LexUnit
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory


data VorN = V | N deriving Eq


data Config = Config { _cfg_sense_inventory_file :: FilePath
                     , _cfg_semlink_file         :: FilePath
                     , _cfg_statistics           :: FilePath
                     , _cfg_wsj_directory        :: FilePath
                     , _cfg_framenet_file        :: FilePath
                     }

makeLenses ''Config
              
cfg = Config { _cfg_sense_inventory_file = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
             , _cfg_semlink_file = "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
             , _cfg_statistics = "run/OntoNotes_propbank_statistics_only_wall_street_journal_verbonly.txt"
             , _cfg_wsj_directory = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
             , _cfg_framenet_file = "/home/wavewave/repo/srcp/HFrameNet/run/FrameNet_ListOfLexUnit.bin"
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


loadFrameNet file = do
  bstr <- BL.readFile file
  let lst = decode bstr :: [LexUnit]
      lexunitdb = foldl' insertLU emptyDB lst
  return lexunitdb
  


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


formatStat :: ((Text,Text),Int) -> String
formatStat ((lma,sens),num) = printf "%20s.%-5s : %5d" lma sens num


senseInstStatistics :: FilePath -> IO (HashMap (Text,Text) Int)
senseInstStatistics basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      sfiles = filter (\x -> takeExtensions x == ".sense") fps

  sinstss <- flip mapM sfiles $ \fp -> do
    txt <- T.IO.readFile fp
    -- print fp
    let lst = T.lines txt
        wss = map T.words lst
    case traverse parseSenseInst wss of
      Left err -> error err
      Right lst -> return lst

  let sinsts = concat sinstss
      sinsts_verb = filter (\s-> T.last (s^.sinst_sense) == 'v') sinsts  
      ks = map (\s -> ( T.init (T.init (s^.sinst_sense)) ,s^.sinst_sense_num)) sinsts_verb
      acc = foldl' (\(!acc) k -> HM.insertWith (+) k 1 acc) HM.empty ks
  -- mapM_ (putStrLn.formatStat) . sortBy (flip compare `on` snd) . HM.toList $ acc
  return acc




main :: IO ()
main = do
  ludb <- loadFrameNet (cfg^.cfg_framenet_file)
   
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
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

