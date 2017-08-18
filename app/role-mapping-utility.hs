{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Lens              hiding (para)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Either                      (rights)
import           Data.Either.Extra                (maybeToEither)
import           Data.Function                    (on)
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import qualified Data.List.Split            as L.S
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Lazy.IO          as T.L.IO
import           Data.Text.Read                   (decimal)
import qualified Options.Applicative        as O
import           System.Console.Haskeline
import           System.Console.Haskeline.MonadException
import           System.Environment
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import           FrameNet.Query.Frame             (frameDB,loadFrameData)
import           FrameNet.Type.Common             (CoreType(..),fr_frame)
import           FrameNet.Type.Frame
import           NLP.Syntax.Type
import           PropBank.Query                   (constructPredicateDB, constructFrameDB
                                                  ,constructRoleSetDB, rolesetDB
                                                  )
import           PropBank.Type.Frame       hiding (Voice,ProgOption)
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS
--
import           OntoNotes.App.Load
import           OntoNotes.Corpus.Load
import           OntoNotes.Format
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.Type.ArgTable
import           OntoNotes.Type.SenseInventory


{- 
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
-}

extractPBRoles pb =
  pb^..roleset_roles.roles_role.traverse.to ((,) <$> (^.role_n) <*> (^.role_descr))

extractPBExamples pb = pb^..roleset_example.traverse.example_text


createONFN subcats sensemap framedb rolesetdb = do 
  (lma, senses ) <- mapFromONtoFN
  (sid,frtxt) <- senses
  let g = T.head sid
      n = T.drop 2 sid
  let lmav = lma <> "-v"
  subcat <- maybeToList (find (\c -> c^._1 == (lmav,n)) subcats)
  
  si <- maybeToList (HM.lookup lmav sensemap)
  osense <- maybeToList $
              find (\s -> T.head (s^.sense_group) == g && s^.sense_n == n)
                   (si^.inventory_senses)
  frame <- maybeToList $ HM.lookup frtxt (framedb^.frameDB)
  let pbids = T.splitOn "," (osense^.sense_mappings.mappings_pb)
      pbs = mapMaybe (\pb -> HM.lookup pb (rolesetdb^.rolesetDB)) pbids
      -- pbroles = map (\pb -> (pb^.roleset_id,pb^.roleset_name,extractPBRoles pb)) pbs
  return (lma,osense,frame,pbs,subcat)


loadPropBankDB = do
  preddb <- constructPredicateDB <$> constructFrameDB (cfg^.cfg_propbank_framedir)
  let rolesetdb = constructRoleSetDB preddb 
  return (preddb,rolesetdb)


formatPBInfos (rid,mname,examples,roles) = printf "%-20s: %s\n" rid (fromMaybe "" mname)
                                           ++ intercalate "\n" rolestrs
                                           ++ "\n-----------\n"
                                           ++ T.unpack (T.intercalate "\n" examples)
                                           ++ "\n-----------"
                                           
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
  in (icorefes,iperifes,iextrafes)




formatFEs (icorefes,iperifes,iextrafes) = formatf icorefes ++ " | " ++ formatf iperifes ++ " | " ++ formatf iextrafes
  where formatf fes = intercalate ", " (map (\(i,fe) -> printf "%2d-%s" (i :: Int) (fe^.fe_name)) fes)


problemID (i,(lma,osense,frame,pbs,_)) = let sid = osense^.sense_group <> "." <> osense^.sense_n
                                         in (lma,sid)


formatProblem :: (Int,_) -> (String,String,String,String,String)
formatProblem (i,(lma,osense,frame,pbs,subcat)) =
  let sid = osense^.sense_group <> "." <> osense^.sense_n
      headstr = printf "%d th item: %s %s" i lma sid
      sensestr = printf "definition: %s\n%s" (osense^.sense_name) (osense^.sense_examples)      
      fes = numberedFEs frame
      framestr = printf "%s" (frame^.frame_name) ++ "\n" ++ formatFEs fes
      argpattstr = intercalate "\n" $ flip map (Prelude.take 10 (subcat^._2)) $ \(patt,n) ->
                     printf "%s     #count: %5d" (formatArgPatt patt) (n :: Int)
      pbinfos = map (\pb -> (pb^.roleset_id,pb^.roleset_name,extractPBExamples pb,extractPBRoles pb)) pbs
      pbinfostr = intercalate "\n" $ map formatPBInfos pbinfos
  in (headstr,sensestr,framestr,argpattstr,pbinfostr)


showProblem :: (Int,_) -> IO ()
showProblem prob = do
  let (headstr,sensestr,framestr,argpattstr,pbinfostr) = formatProblem prob
  putStrLn "========================================================================================================="  
  putStrLn headstr
  putStrLn sensestr
  putStrLn "---------------------------------------------------------------------------------------------------------\n"
  putStrLn framestr
  putStrLn "---------------------------------------------------------------------------------------------------------\n"
  putStrLn argpattstr
  putStrLn "---------------------------------------------------------------------------------------------------------\n"
  putStrLn pbinfostr
  putStrLn "---------------------------------------------------------------------------------------------------------\n"


reformatInput o txt = let ws = T.words txt
                      in T.intercalate " " $ map f ws
  where fes0 = numberedFEs (o^._2._3) 
        fes = fes0^._1 ++ fes0^._2 ++ fes0^._3

        f w = let xs = T.splitOn ":" w
              in case xs of
                   (x:y:[]) -> case decimal y of
                                 Left _ -> w
                                 Right (n,_) -> case find ((== n) . (^._1)) fes of
                                                 Just (_,fe) -> x <> ":" <> fe^.fe_name
                                                 Nothing -> w
                                 

formatResult (i,(lma,sid),frm,txt) = printf "%d\t%s\t%s\t%s\t%s\n" i lma sid frm txt
  

prompt = do
  (result,olst) <- lift get
  case olst of
    [] -> return ()
    o:os -> do
      liftIO $ showProblem o
      m <- getInputLine "? "
      case m of
        Nothing -> return ()
        Just x -> do
          let r1 = reformatInput o (T.pack x)
          liftIO $ T.IO.putStrLn r1
          let nresult = result ++ [(o^._1,problemID o,o^._2._3.frame_name,r1)]
          liftIO $ writeFile "temp.txt" (concatMap formatResult nresult)
          lift (put (nresult,os))
          prompt


data ProgOption = ProgOption { progCommand :: String
                             , startNum :: Maybe Int
                             } deriving Show


pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strArgument (O.help "program command show or tag")
                      <*> optional (O.argument O.auto (O.help "starting number"))

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "role mapping utility program")


main = do
  opt <- O.execParser progOption
  (ludb,sensestat,semlinkmap,sensemap,ws,_) <- loadAllexceptPropBank
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  (preddb,rolesetdb) <- loadPropBankDB
  subcats <- loadVerbSubcat (cfg^.cfg_verb_subcat_file)
  rolemap <- loadRoleMap (cfg^.cfg_rolemap_file)

  let flattened = createONFN subcats sensemap framedb rolesetdb 
  let indexed = zip [1..] flattened

  case progCommand opt of
    "tag" -> do
      let n = fromMaybe 0 (startNum opt)
          indexed_being_processed = (drop (n-1) . take (n+99)) indexed
      r <- flip execStateT (([] :: [(Int,(Text,Text),Text,Text)]),indexed_being_processed) $ runInputT defaultSettings prompt
      print (fst r)
      let filename = "final" ++ show n ++ "-" ++ show (n+length (fst r)-1) ++ ".txt" -- ) (show (fst r))
      writeFile filename (concatMap formatResult (fst r))
    "show" -> do
      flip mapM_ indexed $ \prob -> do
        let (headstr,sensestr,framestr,argpattstr,pbinfostr) = formatProblem prob
        case find (\rm -> let rmid = rm^._1 in rmid == problemID prob) rolemap of
          Nothing -> return ()
          Just rm -> do
            let argmap = rm^._2
            putStrLn "\n\n\n========================================================================================================="  
            putStrLn headstr
            putStrLn sensestr
            putStrLn "---------------------------------------------------------------------------------------------------------"
            putStrLn framestr
            putStrLn "---------------------------------------------------------------------------------------------------------"
            putStrLn $ formatRoleMap argmap
            putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
            putStrLn argpattstr
            putStrLn "========================================================================================================="  
        -- mapM_ showProblem indexed
    cmd -> putStrLn (cmd ++ " cannot be processed")


