{-# LANGUAGE ScopedTypeVariables #-}

module Text.Taggy.Lens.Util where

import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
-- import           System.Directory
-- import           System.FilePath


class FromAttr a where
  fromAttr :: Element -> Text -> Either String a

instance FromAttr Text where
  fromAttr x n = case x ^. attr n of
                   Nothing -> Left (T.unpack n)
                   Just r  -> Right r

instance FromAttr a => FromAttr (Maybe a) where
  fromAttr x n = either (\_ -> Right Nothing) (Right . Just) (fromAttr x n :: Either String a)
  

(.:) :: FromAttr a => Element -> Text -> Either String a
(.:) = fromAttr 


getOnly :: Element -> Text -> [Element] 
getOnly x k = x ^.. elements . named (only k)

getOnly1 :: Element -> Text -> Parser Element
getOnly1 x k = case getOnly x k of
                 [] -> Left (T.unpack k)
                 (x':_) -> Right x'


type Parser = Either String

p_list :: (Element -> Either String a) -> Text -> Text -> Element -> Either String [a]
p_list p ech group x = getOnly1 x group >>= \y -> traverse p (getOnly y ech) 
