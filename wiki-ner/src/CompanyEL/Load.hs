{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CompanyEL.Load where

import           Control.Lens        ( (^.), _2 )
import           Control.Monad       ( forM )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv as C
import           Data.Hashable       ( Hashable )
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.List           ( foldl' )
import           Data.Maybe          ( fromMaybe )
import           Data.Text           ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import           System.FilePath     ( (</>) )
--
import           CompanyEL.Type
-- import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Tagger    ( loadEntityReprs, reprFileG )
import           WikiEL.Type      ( ItemID(..), ItemRepr(..), EntityReprRow(..) )
-- import qualified WikiEL.Type.FileFormat  as FF
-- import qualified WikiEL.Type.Wikidata    as WD


-- companyDataDir = "/data/groups/uphere/data/NER/company"
-- wikiDataDir = "/data/groups/uphere/wikidata"


mkNameUIDHM :: [(ItemID,Text)] -> HashMap Text ItemID
mkNameUIDHM nameTable = foldl' (\acc (i,n) -> HM.insert n i acc) HM.empty nameTable


mkUIDAliasHM :: [(ItemID,Text)] -> HashMap ItemID [Text]
mkUIDAliasHM nameTable = foldl' (\acc (i,n) -> HM.insertWith (\xs1 -> (\xs2 -> xs1 ++ xs2)) i [n] acc) HM.empty nameTable


-- | returns (Wiki UID, Text)
loadNameTable :: FilePath -> IO [(ItemID,Text)]
loadNameTable dataDir = do
  reprs <- loadEntityReprs (reprFileG dataDir)
  pure $ map (\x -> (_row_uid x, (_repr . _row_repr) x)) reprs


aliasFinder :: (Hashable t, Hashable k, Eq t, Eq k) => HashMap t k -> HashMap k a -> t -> Maybe a
aliasFinder nuid uida txt =
  let muid = HM.lookup txt nuid
  in case muid of
    Nothing  -> Nothing
    Just uid -> HM.lookup uid uida


getCompanyListFromJSON :: FilePath -> IO [CompanyInfo]
getCompanyListFromJSON fp = do
  lbstr <- BL8.readFile fp
  let mresult = A.decode lbstr
  case mresult of
    Nothing     -> error "Company list is not valid."
    Just result -> pure result


constructCompanyListFromCSV ::
     (FilePath,FilePath) -- ^ company data dir, wiki data dir
  -> [(ItemID,Text)]
  -> IO [CompanyInfo]
constructCompanyListFromCSV (companyDataDir,wikiDataDir) nameTable = do
  let nuid = mkNameUIDHM nameTable
      uida = mkUIDAliasHM nameTable

  let fps = map (companyDataDir </>) ["AMEX.csv","NASDAQ.csv","NYSE.csv"]
  clist <- fmap concat $ forM fps $ \fp -> do
    txt' <- TIO.readFile fp
    let tlines = T.lines txt'
        txt = T.intercalate "\n" $ map (T.reverse . (T.drop 2) . T.reverse) tlines
        bstr = BL8.pack $ T.unpack txt
    let (ecompany :: Either String (V.Vector CSVListedCompany)) = C.decode C.HasHeader bstr
    case ecompany of
      Left err -> error err
      Right  v -> pure $ V.toList v

  aList <- flip mapM clist $ \c -> do
    let cinfo = \i -> CompanyInfo i
                        (c ^. csvTicker)
                        (c ^. csvName)
                        (fromMaybe [c ^. csvName] $ aliasFinder nuid uida (c ^. csvName))
                        (c ^. csvSector) (c ^. csvIndustry)
    pure cinfo

  bstr <- BL.readFile (wikiDataDir </> "public_companies.csv")
  let ecompany = C.decode C.HasHeader bstr

  -- (wikipage_name,name,wikipage_market,market)
  (clist2 :: [(Text,Text,Text,Text)]) <- case ecompany of
    Left err -> error err
    Right v -> do
      pure (V.toList v)

  aList2 <- flip mapM clist2 $ \c -> do
    let cinfo = \i -> CompanyInfo i ""
                        (c ^. _2)
                        (fromMaybe [c ^. _2] $ aliasFinder nuid uida (c ^. _2))
                        ""
                        ""
    pure cinfo

  pure (zipWith (\x f -> f x) [1..] (aList ++ aList2))
