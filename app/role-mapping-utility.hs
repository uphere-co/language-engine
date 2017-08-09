{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
import           Data.Either                      (rights)
import           Data.Either.Extra                (maybeToEither)
import           Data.Foldable

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
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           FrameNet.Query.Frame             (frameDB,loadFrameData)
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common             (fr_frame)
import           FrameNet.Type.LexUnit
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS
--
import           OntoNotes.App.Load
import           OntoNotes.Corpus.Load
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.Type.SenseInventory



verbnet :: HashMap (Text,Text) [Text] -> Text -> Text -> Box
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


formatSenses :: Text -> VorN -> HashMap Text Inventory -> HashMap (Text,Text) [Text] -> HashMap (Text,Text) Int -> [Box]
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


formatWordNet :: Text -> VorN -> HashMap Text Inventory -> HashMap (Text,Text) Int -> WordNetDB -> [Box]
formatWordNet lma vorn sensemap sensestat wndb = do
  let lmav = lma <> case vorn of V -> "-v" ; N -> "-n"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt1 = text (printf "%2s.%-6s (%4d cases) |  " (s^.sense_group) (s^.sense_n) num )
      mappings = s^.sense_mappings
      wns = mappings^..mappings_wn.traverse
      wndatas = do wn <- wns
                   snumtxt <- listWordNets wn
                   snum <- fst <$> rights [decimal snumtxt]
                   let l = case wn^.wn_lemma of
                             Nothing -> lma
                             Just l' -> l'
                       wnsense = l <> "." <> T.pack (show snum)
                       dbdata = do
                         (snum',soff') <- join . maybeToList $ (indexDB wndb POS_V) ^? at l . _Just . idx_synset_offset
                         d <- maybeToList ((dataDB wndb POS_V) ^. at (unSynsetOffset soff'))
                         return (snum',d)
                   result <- maybeToList (find (\x -> x^._1 == SenseNumber snum) dbdata)
                   return (wnsense,result)
      txt_wn_senses = do (wnsense,(_,d)) <- wndatas
                         let wnverbframes = show (d^..data_frames.traverse.to ((,)<$> view frame_f_num <*> view frame_w_num))
                             restxt = printf "%-20s: %-45s | %25s | %-s  "
                                        wnsense
                                        (T.intercalate "," (map formatLI (d^.data_word_lex_id)))
                                        wnverbframes
                                        (d^.data_gloss)
                         return (text restxt)
      wn_frames = map head . group .  sort $ wndatas^..traverse._2._2.data_frames.traverse.frame_f_num
      txt_wn_frames = text ("All frames: " ++ show wn_frames)
      txt_wn = vcat left (txt_wn_senses ++ [txt_wn_frames])

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
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAllexceptPropBank

  let merged = mergeStatPB2Lemma ws
  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma
    let frms = framesFromLU ludb (lma <> ".v")
        doc = text (printf "%20s:%6d " lma f) //
              vcat top (formatSenses lma V sensemap semlinkmap sensestat )
    putStrLn "====================================================================================================================="
    putStrLn $ "From FrameNet Lexical Unit " ++ show (lma <> ".v") ++ ": " ++ show frms
    putStrLn (render doc)


listSenseWordNet :: IO ()
listSenseWordNet = do
  (_ludb,sensestat,_semlinkmap,sensemap,ws,wndb) <- loadAllexceptPropBank

  {- 
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics) -}
  let merged = mergeStatPB2Lemma ws

  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma
    let doc = text "=====================================================================================================================" //
              text (printf "%20s:%6d " lma f) //
              text "---------------------------------------------------------------------------------------------------------------" //
              vcat top (formatWordNet lma V sensemap sensestat wndb)
    putStrLn (render doc)



main' :: IO ()
main' = do
  let vnfnrole_file= "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VN-FNRoleMapping.txt"
  let pbvn_file= "/scratch/wavewave/SemLink/1.2.2c/vn-pb/vnpbMappings"

  vnfnrole_txt <- T.L.IO.readFile vnfnrole_file
  pbvn_txt <- T.L.IO.readFile pbvn_file

  let e = do 
        vnfnrole_xml <- maybeToEither "xml parsing vnfnrole failed" (vnfnrole_txt ^? html . allNamed (only "verbnetRoles-framenetFEs_RoleMappingData"))
        pbvn_xml     <- maybeToEither "xml parsing pbvn failed"     (pbvn_txt     ^? html . allNamed (only "pbvn-typemap"))
        vnfnrole     <- p_vnfnrolemap vnfnrole_xml
        pbvn         <- p_pbvnmap pbvn_xml
        return (vnfnrole,pbvn)
  case e of
    Left err -> error err
    Right (vnfnrole,pbvn) -> do
      let vnfnmap = HM.fromList (vnfnrole^..vnfnrolemap_vnfnroles.traverse.to ((,) <$> (^.vnfnroleinst_class) <*> id))
          pbvnmap = HM.fromList (pbvn^..pbvnmap_predicates.traverse.to ((,)<$> (^.pbvn_lemma) <*> id))
      print (HM.lookup "40.3.2" vnfnmap)
      let mrun01 = do pbvn <- HM.lookup "run" pbvnmap
                      find (\a->a^.pbvnarg_pbroleset == "run.01") (pbvn^.pbvn_argmap)
          mfn = HM.lookup "51.3.2" vnfnmap

      print mrun01
      print mfn

main = do
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAllexceptPropBank
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  
  let flattened = do
        (lma, senses ) <- mapFromONtoFN
        (sid,frtxt) <- senses
        let g = T.head sid
            n = T.drop 2 sid
        let lmav = lma <> "-v"
        si <- maybeToList (HM.lookup lmav sensemap)
        osense <- maybeToList $
                    find (\s -> T.head (s^.sense_group) == g && s^.sense_n == n)
                         (si^.inventory_senses)
        let frame = HM.lookup frtxt (framedb^.frameDB)
          
        return (lma,osense,frame)
  let indexed = zip [1..] flattened
  -- mapM_ print indexed
  mapM_ print (filter ((== False) . (^._2._4)) indexed )
