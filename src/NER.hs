{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv             as C
import qualified Data.HashMap.Strict  as HM
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import qualified Data.Vector          as V
--
import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Run                    (reprFileG)
import qualified WikiEL.Type.FileFormat  as FF
import qualified WikiEL.Type.Wikidata    as WD

parseCompany = do
  let fp = "/home/modori/temp/companylist.csv"
  txt' <- T.IO.readFile fp -- BL.readFile fp
  let tlines = T.lines txt'
      txt = T.intercalate "\n" $ map (T.reverse . (T.drop 2) . T.reverse) tlines
      bstr = BL8.pack $ T.unpack txt 
  let (ecompany :: Either String (V.Vector (Text,Text,Text,Text,Text,Text,Text,Text))) = C.decode C.HasHeader bstr
  case ecompany of
    Left err -> error err
    Right  v -> do
      let ctable = V.toList v
      print ctable
-- mkCompanyTable1 :: (Text,Text,Text,Text,Text,Text,Text,Text)

aliasFinder = do
  reprs <- LD.loadEntityReprs reprFileG
  let namelist = map (\x -> ((WD._itemID . FF._uid) x,(WD._repr . FF._repr) x)) reprs
  print $ filter (\(i,x) -> i == 312) namelist
