{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER.Load where

import           Control.Lens               ((^.),_2)
import           Control.Monad              (forM)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv             as C
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict  as HM
import           Data.List                  (foldl')
import           Data.Maybe                 (fromMaybe)
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




mkNameUIDHM :: [(Int,Text)] -> HM.HashMap Text Int
mkNameUIDHM nameTable = foldl' (\acc (i,n) -> HM.insert n i acc) HM.empty nameTable


mkUIDAliasHM :: [(Int,Text)] -> HM.HashMap Int [Text]
mkUIDAliasHM nameTable = foldl' (\acc (i,n) -> HM.insertWith (\xs1 -> (\xs2 -> xs1 ++ xs2)) i [n] acc) HM.empty nameTable


-- | returns (Wiki UID, Text)
loadNameTable :: IO [(Int,Text)]
loadNameTable = do
  reprs <- LD.loadEntityReprs reprFileG
  return $ map (\x -> ((WD._itemID . FF._uid) x,(WD._repr . FF._repr) x)) reprs


loadCompanies :: IO [CompanyInfo]
loadCompanies = do
  nt <- loadNameTable
  companies <- getCompanyList nt
  return companies


aliasFinder :: (Hashable t, Hashable k, Eq t, Eq k) => HM.HashMap t k -> HM.HashMap k a -> t -> Maybe a
aliasFinder nuid uida txt =
  let muid = HM.lookup txt nuid
  in case muid of
    Nothing  -> Nothing
    Just uid -> HM.lookup uid uida


getCompanyList :: [(Int,Text)] -> IO [CompanyInfo]
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

  bstr <- BL.readFile "/data/groups/uphere/wikidata/public_companies.csv"
  let ecompany = C.decode C.HasHeader bstr

  -- (wikipage_name,name,wikipage_market,market)
  (clist2 :: [(Text,Text,Text,Text)]) <- case ecompany of
    Left err -> error err
    Right v -> do
      return (V.toList v)

  aList2 <- flip mapM clist2 $ \c -> do
    let cinfo = CompanyInfo "" (c ^. _2) (fromMaybe [c ^. _2] $ aliasFinder nuid uida (c ^. _2)) "" ""
    return cinfo

  return (aList ++ aList2)
