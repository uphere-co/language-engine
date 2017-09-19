{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Applicative
import           Control.Lens              hiding (para)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import           Data.Text.Read                   (decimal)
import qualified Options.Applicative        as O
import           System.Console.Haskeline
import           Text.Printf
--
import           FrameNet.Query.Frame             (FrameDB,frameDB,loadFrameData)
import           FrameNet.Type.Common             (CoreType(..))
import           FrameNet.Type.Frame
import           Lexicon.Format                   (formatArgPatt,formatRoleMap,formatRoleMapTSV)
import           Lexicon.Mapping.OntoNotesFrameNet (mapFromONtoFN)
import           Lexicon.Query
import           Lexicon.Type                     (POSVorN(..),RoleInstance,RolePattInstance,SenseID)
import           NLP.Type.SyntaxProperty          (Voice)
import           OntoNotes.Type.SenseInventory
import           PropBank.Query                   (PredicateDB,RoleSetDB
                                                  ,constructPredicateDB, constructFrameDB
                                                  ,constructRoleSetDB, rolesetDB
                                                  )
import           PropBank.Type.Frame       hiding (Voice,ProgOption)
--
import           Lexicon.Data



extractPBRoles :: RoleSet -> [(Text,Maybe Text)]
extractPBRoles pb =
  pb^..roleset_roles.roles_role.traverse.to ((,) <$> (^.role_n) <*> (^.role_descr))


extractPBExamples :: RoleSet -> [Text]
extractPBExamples pb = pb^..roleset_example.traverse.example_text


createONFN :: [RolePattInstance Voice]
           -> HashMap Text Inventory
           -> FrameDB
           -> RoleSetDB
           -> [(Text,Sense,Frame,[RoleSet],RolePattInstance Voice)]
createONFN subcats sensemap framedb rolesetdb = do
  (lma, senses ) <- mapFromONtoFN
  (sid,frtxt) <- senses
  let g = T.head sid
      n = T.drop 2 sid
      lmav = lma <> "-v"
  subcat <- maybeToList (find (\c -> c^._1 == (lma,Verb,sid)) subcats)
  si <- maybeToList (HM.lookup lmav sensemap)
  osense <- maybeToList $
              find (\s -> T.head (s^.sense_group) == g && s^.sense_n == n)
                   (si^.inventory_senses)
  frame <- maybeToList $ HM.lookup frtxt (framedb^.frameDB)
  let pbids = T.splitOn "," (osense^.sense_mappings.mappings_pb)
      pbs = mapMaybe (\pb -> HM.lookup pb (rolesetdb^.rolesetDB)) pbids
  return (lma,osense,frame,pbs,subcat)


loadPropBankDB :: IO (PredicateDB,RoleSetDB)
loadPropBankDB = do
  preddb <- constructPredicateDB <$> constructFrameDB (cfg^.cfg_propbank_framedir)
  let rolesetdb = constructRoleSetDB preddb
  return (preddb,rolesetdb)


formatPBInfos :: (Text,Maybe Text,[Text],[(Text,Maybe Text)]) -> String
formatPBInfos (rid,mname,examples,roles) = printf "%-20s: %s\n" rid (fromMaybe "" mname)
                                           ++ intercalate "\n" rolestrs
                                           ++ "\n-----------\n"
                                           ++ T.unpack (T.intercalate "\n" examples)
                                           ++ "\n-----------"

  where
    rolestrs = flip map roles $ \(n,mdesc) -> printf "arg%1s: %s" n (fromMaybe "" mdesc)


numberedFEs :: Frame -> ([(Int,FE)],[(Int,FE)],[(Int,FE)])
numberedFEs frame =
  let fes = frame^..frame_FE.traverse
      corefes = filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed) fes
      perifes = filter (\fe -> fe^.fe_coreType == Peripheral) fes
      extrafes = filter (\fe -> fe^.fe_coreType == ExtraThematic) fes
      icorefes = zip [0..] corefes
      iperifes = zip [length corefes..] perifes
      iextrafes = zip [length corefes+length perifes..] extrafes
  in (icorefes,iperifes,iextrafes)


formatFEs :: ([(Int,FE)],[(Int,FE)],[(Int,FE)]) -> String
formatFEs (icorefes,iperifes,iextrafes) = formatf icorefes ++ " | " ++ formatf iperifes ++ " | " ++ formatf iextrafes
  where formatf fes = intercalate ", " (map (\(i,fe) -> printf "%2d-%s" (i :: Int) (fe^.fe_name)) fes)


problemID :: (Int,(Text,Sense,Frame,[RoleSet],RolePattInstance Voice)) -> SenseID
problemID (_,(lma,osense,_frame,_pbs,_)) = let sid = osense^.sense_group <> "." <> osense^.sense_n
                                           in (lma,Verb,sid)


formatProblem :: (Int,(Text,Sense,Frame,[RoleSet],RolePattInstance Voice))
              -> (String,String,String,String,String)
formatProblem (i,(lma,osense,frame,pbs,subcat)) =
  let sid = osense^.sense_group <> "." <> osense^.sense_n
      headstr = printf "%d th item: %s %s" i lma sid
      sensestr = printf "definition: %s\n%s" (osense^.sense_name) (osense^.sense_examples)
      fes = numberedFEs frame
      framestr = printf "%s" (frame^.frame_name) ++ "\n" ++ formatFEs fes
      argpattstr = intercalate "\n" $ flip map (Prelude.take 10 (subcat^._2)) $ \(patt,n) ->
                     printf "%s     #count: %5d" (formatArgPatt "voice" patt) (n :: Int)
      pbinfos = map (\pb -> (pb^.roleset_id,pb^.roleset_name,extractPBExamples pb,extractPBRoles pb)) pbs
      pbinfostr = intercalate "\n" $ map formatPBInfos pbinfos
  in (headstr,sensestr,framestr,argpattstr,pbinfostr)


showProblem :: (Int,(Text,Sense,Frame,[RoleSet],RolePattInstance Voice)) -> IO ()
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


mkRoleInstance :: (Int,(Text,Sense,Frame,[RoleSet],RolePattInstance Voice)) -> Text -> [(Text,Text)]
mkRoleInstance o txt =
    let parsed = (map f . T.words) txt
        (causes,rest) = partition (\(k,_) -> k == "cause") parsed
    in if null causes
       then ("frame",frm):("cause","single"):rest
       else ("frame",frm):("cause",(head causes)^._2):rest

  where frm = o^._2._3.frame_name
        fes0 = numberedFEs (o^._2._3)
        fes = fes0^._1 ++ fes0^._2 ++ fes0^._3

        f w = let xs = T.splitOn ":" w
              in case xs of
                   (x:y:[]) -> case decimal y of
                                 Left _ -> (w,"")
                                 Right (n,_) -> case find ((== n) . (^._1)) fes of
                                                 Just (_,fe) -> (x,fe^.fe_name)
                                                 Nothing -> (w,"")
                   (x:[]) -> if | x == "dual"   -> ("cause","dual")
                                | x == "single" -> ("cause","single")
                                | otherwise     -> (x,"")
                   _ -> error "mkRoleInstance"


prompt :: InputT (StateT ([(Int,(SenseID,[(Text,Text)]))],[(Int,(Text,Sense,Frame,[RoleSet],RolePattInstance Voice))]) IO) ()
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
          let r1 = mkRoleInstance o (T.pack x)
          liftIO $ print r1
          let nresult = result ++ [(o^._1,(problemID o,r1))]
          liftIO $ writeFile "temp.txt" (concatMap formatRoleMapTSV nresult)
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


main :: IO ()
main = do
  opt <- O.execParser progOption
  (_ludb,_sensestat,_semlinkmap,sensemap,_ws,_) <- loadAllexceptPropBank cfg
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  (_preddb,rolesetdb) <- loadPropBankDB

  (subcats :: [RolePattInstance Voice]) <- loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  (rolemap :: [RoleInstance]) <- loadRoleInsts (cfg^.cfg_rolemap_file)

  let flattened = createONFN subcats sensemap framedb rolesetdb
  let iflattened = zip [1..] flattened

  case progCommand opt of
    "tag" -> do
      let n = fromMaybe 0 (startNum opt)
          iflattened_being_processed = (drop (n-1) . take (n+99)) iflattened
      r <- flip execStateT (([] :: [(Int,RoleInstance)]),iflattened_being_processed) $ runInputT defaultSettings prompt
      print (fst r)
      let filename = "final" ++ show n ++ "-" ++ show (n+length (fst r)-1) ++ ".txt"
      writeFile filename (concatMap formatRoleMapTSV (fst r))
    "show" -> do
      flip mapM_ iflattened $ \prob -> do
        let (headstr,sensestr,framestr,argpattstr,_pbinfostr) = formatProblem prob
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
    cmd -> putStrLn (cmd ++ " cannot be processed")
