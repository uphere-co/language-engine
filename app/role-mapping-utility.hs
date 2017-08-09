{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens              hiding (para)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
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
import           System.Console.Haskeline
import           System.Console.Haskeline.MonadException
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           FrameNet.Query.Frame             (frameDB,loadFrameData)
import           FrameNet.Type.Common             (CoreType(..),fr_frame)
import           FrameNet.Type.Frame
import           PropBank.Query                   (constructPredicateDB, constructFrameDB
                                                  ,constructRoleSetDB, rolesetDB
                                                  )
import           PropBank.Type.Frame
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

{- 
framesFromLU :: LexUnitDB -> Text -> [Text]
framesFromLU ludb lma = do
  i <- fromMaybe [] (HM.lookup lma (ludb^.nameIDmap))
  l <- maybeToList (IM.lookup i (ludb^.lexunitDB))
  frm <- maybeToList (l^.lexunit_frameReference^.fr_frame)
  return frm
-}

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





listSenseDetail :: IO ()
listSenseDetail = do
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAllexceptPropBank

  let merged = mergeStatPB2Lemma ws
  forM_ merged $ \(lma,f) -> do
    T.IO.hPutStrLn stderr lma
    -- let frms = framesFromLU ludb (lma <> ".v")
    let doc = text (printf "%20s:%6d " lma f) //
              vcat top (formatSenses lma V sensemap semlinkmap sensestat )
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


extractPBRoles pb =
  pb^..roleset_roles.roles_role.traverse.to ((,) <$> (^.role_n) <*> (^.role_descr))

createONFN sensemap framedb rolesetdb = do 
  (lma, senses ) <- mapFromONtoFN
  (sid,frtxt) <- senses
  let g = T.head sid
      n = T.drop 2 sid
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  osense <- maybeToList $
              find (\s -> T.head (s^.sense_group) == g && s^.sense_n == n)
                   (si^.inventory_senses)
  frame <- maybeToList $ HM.lookup frtxt (framedb^.frameDB)
  let pbids = T.splitOn "," (osense^.sense_mappings.mappings_pb)
      pbs = mapMaybe (\pb -> HM.lookup pb (rolesetdb^.rolesetDB)) pbids
      pbroles = map (\pb -> (pb^.roleset_id,extractPBRoles pb)) pbs
  return (lma,osense,frame,pbroles)


loadPropBankDB = do
  preddb <- constructPredicateDB <$> constructFrameDB (cfg^.cfg_propbank_framedir)
  let rolesetdb = constructRoleSetDB preddb 
  return (preddb,rolesetdb)


formatPBRoles (rid,roles) = T.unpack rid ++ "\n" ++ intercalate "\n" rolestrs ++ "\n-----------"
  where
    rolestrs = flip map roles $ \(n,mdesc) -> printf "arg%1s: %s" n (fromMaybe "" mdesc)

numberedFEs frame = 
  let fes = frame^..frame_FE.traverse
      corefes = filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed) fes
      perifes = filter (\fe -> fe^.fe_coreType == Peripheral) fes
      extrafes = filter (\fe -> fe^.fe_coreType == ExtraThematic) fes
      icorefes = zip [0..] corefes
      iperifes = zip [length corefes..] perifes
      iextrafes = zip [length corefes+length perifes..] extrafes
      -- fes_ordered = corefes ++ perifes ++ extrafes
      -- fes_ordered_txts = zip [1..] . map (^.fe_name) $ fes_ordered
  in (icorefes,iperifes,iextrafes) --  fes_ordered_txts
        -- sid = osense^.sense_group <> osense^.sense_n





formatFEs (icorefes,iperifes,iextrafes) = formatf icorefes ++ " | " ++ formatf iperifes ++ " | " ++ formatf iextrafes
  where formatf fes = intercalate ", " (map (\(i,fe) -> printf "%2d-%s" (i :: Int) (fe^.fe_name)) fes)


showProblem :: (Int,_) -> InputT (StateT _ IO) ()
showProblem (i,(lma,osense,frame,pbroles)) = do
  let sid = osense^.sense_group <> "." <> osense^.sense_n
  liftIO $ putStrLn "========================================================================================================="  
  liftIO $ putStrLn (printf "%d th item: %s %s" i lma sid)
  let fes = numberedFEs frame
  liftIO $ putStrLn "---------------------------------------------------------------------------------------------------------"  
  liftIO $ putStrLn (formatFEs fes)
  liftIO $ putStrLn "---------------------------------------------------------------------------------------------------------"
  mapM_ (liftIO . putStrLn . formatPBRoles) pbroles
  liftIO $ putStrLn "---------------------------------------------------------------------------------------------------------"

prompt = do
  (rlst,olst) <- lift get
  case olst of
    [] -> return ()
    o:os -> do
      showProblem o
      m <- getInputLine "? "
      case m of
        Nothing -> return ()
        Just x -> do
          lift (put (rlst,os))
          prompt



main = do
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAllexceptPropBank
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  (preddb,rolesetdb) <- loadPropBankDB
  -- print (HM.lookup "examine.01" (rolesetdb^.rolesetDB))
  
  let flattened = createONFN sensemap framedb rolesetdb 
  let indexed = zip [1..] flattened
        
{- 
    
    print (i,lma,sid,fes_ordered_txts)
    mapM_ (putStrLn . formatPBRoles) pbroles
  -- mapM_ print (filter ((== False) . (^._2._4)) indexed )
-}
  r <- flip runStateT (([] :: [Int]),indexed) $ runInputT defaultSettings prompt
  print r
--     minput <- getInputLine "? "
    
    



{- 
  whileJust_ (getInputLine "? ") $ \input' -> liftIO $ do
  forM_ (take 20 indexed) $ \(i,(lma,osense,frame,pbroles)) -> do
    let fes = frame^..frame_FE.traverse
        corefes = filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed) fes
        perifes = filter (\fe -> fe^.fe_coreType == Peripheral) fes
        extrafes = filter (\fe -> fe^.fe_coreType == ExtraThematic) fes
        fes_ordered = corefes ++ perifes ++ extrafes
        fes_ordered_txts = zip [1..] . map (^.fe_name) $ fes_ordered

        sid = osense^.sense_group <> osense^.sense_n
        -- proptxts = map (rs^..roleset_roles.roles_role.traverse.to ((,) <$> ^.role_n, ) propbanks
        

    
    print (i,lma,sid,fes_ordered_txts)
    mapM_ (putStrLn . formatPBRoles) pbroles
  -- mapM_ print (filter ((== False) . (^._2._4)) indexed )
-}
