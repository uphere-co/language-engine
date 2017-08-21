{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
import           Data.Either                      (rights)
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
import           Data.Text.Read                   (decimal)
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common             (fr_frame)
import           FrameNet.Type.LexUnit
import           Lexicon.Type                     (POSVorN(..))
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS                 (POS(..))
--
import           OntoNotes.App.Load
import           OntoNotes.Corpus.Load
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


formatSenses :: Text -> POSVorN -> HashMap Text Inventory -> HashMap (Text,Text) [Text] -> HashMap (Text,Text) Int
             -> [Box]
formatSenses lma vorn sensemap semlinkmap sensestat = do
  let lmav = lma <> case vorn of Verb -> "-v" ; Noun -> "-n"
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
                 Verb -> vcat top $
                           let lst = maybe [] (map (verbnet semlinkmap lma) . T.splitOn ",") (mappings^.mappings_vn)
                           in if null lst
                              then [text (printf "%-43s" ("" :: String))]
                              else lst --  map (text.printf "%-20s") lst
                 Noun -> vcat top [text (printf "%-42s" ("" :: String))]
      txt_definition = text ("definition: " ++ T.unpack (s^.sense_name))
      txt_commentary = text (T.unpack (fromMaybe "" (s^.sense_commentary)))
      txt_examples   = text (T.unpack (s^.sense_examples))
      txt_detail = vcat left [txt_definition,txt_commentary,txt_examples]
  return (vcat left [(txt1 <+> txt_pb <+> txt_fn <+> txt_vn <+> txt_wn),txt_detail])


listWordNets :: WN -> [Text]
listWordNets wn = T.splitOn "," (wn^.wn_contents)


formatWordNet :: Text -> POSVorN -> HashMap Text Inventory -> HashMap (Text,Text) Int -> WordNetDB -> [Box]
formatWordNet lma vorn sensemap sensestat wndb = do
  let lmav = lma <> case vorn of Verb -> "-v" ; Noun -> "-n"
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
              vcat top (formatSenses lma Verb sensemap semlinkmap sensestat )
    putStrLn "====================================================================================================================="
    putStrLn $ "From FrameNet Lexical Unit " ++ show (lma <> ".v") ++ ": " ++ show frms
    putStrLn (render doc)


listSenseWordNet :: IO ()
listSenseWordNet = do
  (_ludb,sensestat,_semlinkmap,sensemap,ws,wndb) <- loadAllexceptPropBank

  let merged = mergeStatPB2Lemma ws

  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma
    let doc = text "=====================================================================================================================" //
              text (printf "%20s:%6d " lma f) //
              text "---------------------------------------------------------------------------------------------------------------" //
              vcat top (formatWordNet lma Verb sensemap sensestat wndb)
    putStrLn (render doc)


main :: IO ()
main = listSenseWordNet
