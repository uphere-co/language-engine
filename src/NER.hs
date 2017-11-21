{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NER where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv             as C
import qualified Data.HashMap.Strict  as HM
import           Data.List                  (find)
import           Data.Text                  (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import qualified Data.Vector          as V
--
import qualified WikiEL.ETL.LoadData     as LD
import           WikiEL.Run                    (reprFileG)
import qualified WikiEL.Type.FileFormat  as FF
import qualified WikiEL.Type.Wikidata    as WD

loadNameTable = do
  reprs <- LD.loadEntityReprs reprFileG
  return $ map (\x -> ((WD._itemID . FF._uid) x,(WD._repr . FF._repr) x)) reprs

parseCompany nt = do
  let fp = "/home/modori/temp/companylist.csv"
  txt' <- T.IO.readFile fp -- BL.readFile fp
  let tlines = T.lines txt'
      txt = T.intercalate "\n" $ map (T.reverse . (T.drop 2) . T.reverse) tlines
      bstr = BL8.pack $ T.unpack txt 
  let (ecompany :: Either String (V.Vector (Text,Text,Text,Text,Text,Text,Text,Text))) = C.decode C.HasHeader bstr
  case ecompany of
    Left err -> error err
    Right  v -> do
      let -- w x = (T.tail . T.init) x 
          ctable = V.toList v -- map (\(a,b,c,d,e,f,g,h) -> (w a,w b,w c,w d,w e,w f,w g,w h)) $ V.toList v
      flip mapM_ ctable $ \(_,name,_,_,_,_,_,_) -> do
        print $ (name, aliasFinder nt name)
-- mkCompanyTable1 :: (Text,Text,Text,Text,Text,Text,Text,Text)

uidFinder nt txt = fmap fst $ find (\(_,x) -> x == txt) nt

aliasFinder nt txt =
  let muid = uidFinder nt txt
  in case muid of
    Nothing  -> []
    Just uid -> filter (\(i,_) -> i == uid) nt
