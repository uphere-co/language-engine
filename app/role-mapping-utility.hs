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
import           Lexicon.Format                   (formatArgPatt,formatRoleMap)
import           Lexicon.Mapping.OntoNotesFrameNet (mapFromONtoFN)
import           Lexicon.Query
import           Lexicon.Type                     (ArgPattern(..),POSVorN(..)
                                                  ,RoleInstance,RolePattInstance)
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
--
import           OntoNotes.App.Load
import           OntoNotes.Corpus.Load
import           OntoNotes.Type.SenseInventory



extractPBRoles pb =
  pb^..roleset_roles.roles_role.traverse.to ((,) <$> (^.role_n) <*> (^.role_descr))

extractPBExamples pb = pb^..roleset_example.traverse.example_text


createONFN subcats sensemap framedb rolesetdb = do 
  (lma, senses ) <- mapFromONtoFN
  (sid,frtxt) <- senses
  let g = T.head sid
      n = T.drop 2 sid
  let lmav = lma <> "-v"
  subcat <- maybeToList (find (\c -> c^._1 == (lma,Verb,sid)) subcats)
  
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
                                         in (lma,Verb,sid)


formatProblem :: (Int,_) -> (String,String,String,String,String)
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


reformatInput o = T.intercalate " " . map f . T.words
  where fes0 = numberedFEs (o^._2._3) 
        fes = fes0^._1 ++ fes0^._2 ++ fes0^._3

        f w = let xs = T.splitOn ":" w
              in case xs of
                   (x:y:[]) -> case decimal y of
                                 Left _ -> w
                                 Right (n,_) -> case find ((== n) . (^._1)) fes of
                                                 Just (_,fe) -> x <> ":" <> fe^.fe_name
                                                 Nothing -> w
                                 

formatResult (i,(lma,_,sid),frm,txt) = printf "%d\t%s\t%s\t%s\t%s\n" i lma sid frm txt
  

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
  
  (subcats :: [RolePattInstance Voice]) <- loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  (rolemap :: [RoleInstance]) <- loadRoleInsts (cfg^.cfg_rolemap_file)

  let flattened = createONFN subcats sensemap framedb rolesetdb 
  let indexed = zip [1..] flattened

  case progCommand opt of
    "tag" -> do
      let n = fromMaybe 0 (startNum opt)
          indexed_being_processed = (drop (n-1) . take (n+99)) indexed
      r <- flip execStateT (([] :: [(Int,(Text,POSVorN,Text),Text,Text)]),indexed_being_processed) $ runInputT defaultSettings prompt
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


