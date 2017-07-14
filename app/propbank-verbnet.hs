{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List                     (isSuffixOf,sort)
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
import qualified Data.Text.Lazy.IO   as T.L.IO
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath
import           Text.Printf
--
import           PropBank.Query
import           PropBank.Type.Frame
import           VerbNet.Parser
import           VerbNet.Type

data VNClassContent = VNC { _vnc_members :: [Member]
                          , _vnc_them_roles :: [ThemRole]
                          , _vnc_frames :: [Frame] }
                    deriving (Show)



addaction dir f acc = do
  let fp = dir  </> f
  -- print fp
  txt <- T.L.IO.readFile fp
  case txt ^? html . allNamed (only "VNCLASS") of
    Nothing -> error "nothing"
    Just f -> case p_vnclass f of
                Left err -> error err
                Right c  -> do
                  putStrLn "----------------"
                  let allclasses = let i0 = c^.vnclass_id
                                       vnc0 = VNC (c^.vnclass_members)
                                                  (c^.vnclass_themroles)
                                                  (c^.vnclass_frames)
                                   in ((i0,vnc0) : go (c^.vnclass_subclasses))
                      go [] = []
                      go lst = do x <- lst
                                  let i = x^.vnsubclass_id
                                      vnc = VNC (x^.vnsubclass_members)
                                                (x^.vnsubclass_themroles)
                                                (x^.vnsubclass_frames)
                                  ((i,vnc) : go (x^.vnsubclass_subclasses))

                  return (foldl' (\(!acc') (k,v) -> HM.insert k v acc') acc allclasses)
                  -- let cmap = HM.fromList allclasses -- (c^.vnclass_id)
                  -- print (HM.lookup "bump-18.4" cmap)
                  -- print (c^..vnclass_subclasses.traverse.vnsubclass_id)


main :: IO ()
main = do
  let dir= "/scratch/wavewave/VerbNet/verbnet"
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> ".xml" `isSuffixOf` takeExtensions x) cnts) 
  -- let fs = [ "admit-65.xml" ] -- [ "get-13.5.1.xml" ]
  clmap <- foldrM (addaction dir) HM.empty fs

  print (HM.lookup "bump-18.4" clmap)

getVNCls rdb k = do 
  r <- maybeToList (HM.lookup k (rdb^.rolesetDB))
  r^..roleset_roles.roles_role.traverse.role_vnrole.traverse.vnrole_vncls




main2 :: IO ()
main2 = do
  let dir = "/home/wavewave/repo/srcc/propbank-frames/frames"
  fdb <- constructFrameDB dir
  let pdb = constructPredicateDB fdb
      rdb = constructRoleSetDB pdb
  let ks = HM.keys (rdb^.rolesetDB)

  txt <- T.IO.readFile "OntoNotes_propbank_statistics_only_wall_street_journal.txt"
  let ws = map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)

  forM_ ws $ \(l,f) -> do
    let cls = getVNCls rdb l
        cls_dedupe = HS.toList (HS.fromList cls)
    putStrLn (printf "%20s : %6s :  %s" l f (T.intercalate ", " cls_dedupe))
  

{-
main = do
  main2 
-}
