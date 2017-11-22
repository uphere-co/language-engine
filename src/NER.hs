{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv             as C
import qualified Data.HashMap.Strict  as HM
import           Data.List                  (find,foldl')
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import qualified Data.Vector          as V
--
import           NER.Type
import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Run                    (reprFileG)
import qualified WikiEL.Type.FileFormat  as FF
import qualified WikiEL.Type.Wikidata    as WD

loadNameTable = do
  reprs <- LD.loadEntityReprs reprFileG
  return $ map (\x -> ((WD._itemID . FF._uid) x,(WD._repr . FF._repr) x)) reprs

loadNameHM :: IO (HM.HashMap Text Int)
loadNameHM = do
  reprs <- LD.loadEntityReprs reprFileG
  let nameTable = map (\x -> ((WD._itemID . FF._uid) x, (WD._repr . FF._repr) x)) reprs
  return $ foldl' (\acc (i,n) -> HM.insert n i acc) HM.empty nameTable

-- loadUIDInfo :: IO (HM.HashMap Int Text)
-- loadUIDInfo = do
  
  
parseCompany nt = do
  nhm <- loadNameHM
  let fp = "/home/modori/temp/companylist.csv"
  txt' <- T.IO.readFile fp
  let tlines = T.lines txt'
      txt = T.intercalate "\n" $ map (T.reverse . (T.drop 2) . T.reverse) tlines
      bstr = BL8.pack $ T.unpack txt 
  let (ecompany :: Either String (V.Vector Company)) = C.decode C.HasHeader bstr
  case ecompany of
    Left err -> error err
    Right  v -> do
      let ctable = V.toList v
      flip mapM_ ctable $ \c -> do
        print $ HM.lookup (_name c) nhm 
      {- flip mapM_ ctable $ \(_,name,_,_,_,_,_,_) -> do
        print $ (name, aliasFinder nt name)
      -}
uidFinder nt txt = fmap fst $ find (\(_,x) -> x == txt) nt

aliasFinder nt txt =
  let muid = uidFinder nt txt
  in case muid of
    Nothing  -> []
    Just uid -> filter (\(i,_) -> i == uid) nt
