{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Parser.SenseInventory where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.List                 (sort)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath


class FromAttr a where
  fromAttr :: Element -> Text -> Either String a

instance FromAttr Text where
  fromAttr x n = case x ^. attr n of
                   Nothing -> Left (T.unpack n)
                   Just r  -> Right r

{- 
instance FromAttr a => FromAttr (Maybe a) where
  fromAttr x n = either (\_ -> Right Nothing) (Right . Just) (fromAttr x n :: Either String a)
-}

(.:) :: FromAttr a => Element -> Text -> Either String a
(.:) = fromAttr 


getOnly :: Element -> Text -> [Element] 
getOnly x k = x ^.. elements . named (only k)


getOnly1 :: Element -> Text -> Parser Element
getOnly1 x k = case getOnly x k of
                 [] -> Left (T.unpack k)
                 (x':_) -> Right x'


type Parser = Either String


data Inventory = Inventory { -- _inventory_commentary
                           --   , _inventory_senses
                             _inventory_lemma :: Text
                           }
               deriving Show

makeLenses ''Inventory


p_inventory :: Element -> Parser Inventory
p_inventory x = Inventory <$> x .: "lemma"

main :: IO ()
main = do
  let dir= "/scratch/wavewave/LDC/ontonotes-release-5.0/data/files/data/english/metadata/sense-inventories"
  -- cnts <- getDirectoryContents dir
  --let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts) 
  let fs = [ "get-v.xml" ] -- [ "admit-65.xml" ] -- [ "get-13.5.1.xml" ]
  flip mapM_ fs $ \f -> do
    let fp = dir  </> f
    print fp
    txt <- TLIO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just f -> case p_inventory f of
                  Left err -> error err
                  Right c  -> print c


