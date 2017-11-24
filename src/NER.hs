{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER where

import           Control.Lens               ((^.))
import           Control.Monad              (forM)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv             as C
import qualified Data.HashMap.Strict  as HM
import           Data.List                  (find,foldl')
import           Data.Maybe                 (catMaybes,fromMaybe)
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

mkNameUIDHM :: [(Int,Text)] -> HM.HashMap Text Int
mkNameUIDHM nameTable = foldl' (\acc (i,n) -> HM.insert n i acc) HM.empty nameTable

mkUIDAliasHM :: [(Int,Text)] -> HM.HashMap Int [Text]
mkUIDAliasHM nameTable = foldl' (\acc (i,n) -> HM.insertWith (\xs1 -> (\xs2 -> xs1 ++ xs2)) i [n] acc) HM.empty nameTable

getCompanyList nameTable = do
  let nuid = mkNameUIDHM nameTable
      uida = mkUIDAliasHM nameTable

  let fps = ["/home/modori/temp/AMEX.csv","/home/modori/temp/NASDAQ.csv","/home/modori/temp/NYSE.csv"]
  clist <- fmap concat $ forM fps $ \fp -> do
    txt' <- T.IO.readFile fp
    let tlines = T.lines txt'
        txt = T.intercalate "\n" $ map (T.reverse . (T.drop 2) . T.reverse) tlines
        bstr = BL8.pack $ T.unpack txt 
    let (ecompany :: Either String (V.Vector CSVListedCompany)) = C.decode C.HasHeader bstr
    case ecompany of
      Left err -> error err
      Right  v -> return $ V.toList v

  aList <- flip mapM clist $ \c -> do
    let cinfo = CompanyInfo (c ^. csvTicker) (c ^. csvName) (fromMaybe [c ^. csvName] $ aliasFinder nuid uida (c ^. csvName)) (c ^. csvSector) (c ^. csvIndustry)
    return cinfo


  return aList


saveCompanyInfo = do
  nt <- loadNameTable
  clist <- getCompanyList nt
  print clist

parseCompany nameTable = getCompanyList nameTable >>= print

aliasFinder nuid uida txt =
  let muid = HM.lookup txt nuid
  in case muid of
    Nothing  -> Nothing
    Just uid -> HM.lookup uid uida
