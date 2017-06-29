{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VerbNet where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
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

data Member = Member { _member_name     :: Text
                     , _member_wn       :: Text
                     , _member_grouping :: Maybe Text
                     }
            deriving Show

makeLenses ''Member                     

data ThemRole = ThemRole { _themrole_type :: Text
                         }
            deriving Show

makeLenses ''ThemRole                     


data Description = Description { _desc_primary           :: Text
                               , _desc_secondary         :: Maybe Text
                               , _desc_descriptionNumber :: Text
                               , _desc_xtag              :: Text
                               }
                 deriving Show

makeLenses ''Description

{-
data Syntax = NP
            | VERB
            | ADJ
-}            


data Frame = Frame { _frame_description :: Description
                   , _frame_examples    :: [Text]
                   -- , _frame_syntax      :: [Syntax]
                   }
           deriving (Show)


makeLenses ''Frame                    



data VNSubclass = VNSubclass { _vnsubclass_members   :: [Member]
                             , _vnsubclass_themroles :: [ThemRole]
                             , _vnsubclass_frames    :: [Frame]
                             , _vnsubclass_id        :: Text
                             }
                deriving Show

makeLenses ''VNSubclass                         


data VNClass = VNClass { _vnclass_members    :: [Member]
                       , _vnclass_themroles  :: [ThemRole]
                       , _vnclass_frames     :: [Frame]
                       , _vnclass_subclasses :: [VNSubclass]
                       , _vnclass_id         :: Text
                       }
             deriving Show

makeLenses ''VNClass                        


getOnly :: Element -> Text -> [Element] 
getOnly x k = x ^.. elements . named (only k)

getOnly1 :: Element -> Text -> Parser Element
getOnly1 x k = case getOnly x k of
                 [] -> Left (T.unpack k)
                 (x':_) -> Right x'


type Parser = Either String


p_list p each group x = getOnly1 x group >>= \y -> traverse p (getOnly y each) 

p_member :: Element -> Parser Member
p_member x = Member <$> x .: "name"
                    <*> x .: "wn"
                    <*> optional (x .: "grouping")


p_themrole :: Element -> Parser ThemRole
p_themrole x = ThemRole <$> x .: "type"

p_description :: Element -> Parser Description
p_description x = Description <$> x .: "primary"
                              <*> optional (x .: "secondary")
                              <*> x .: "descriptionNumber"
                              <*> x .: "xtag"
                              

p_frame :: Element -> Parser Frame
p_frame x = Frame <$> (p_description =<< getOnly1 x "DESCRIPTION")
                  <*> p_list (pure . (^.contents)) "EXAMPLE" "EXAMPLES" x


p_vnsubclass :: Element -> Parser VNSubclass
p_vnsubclass x = VNSubclass <$> p_list p_member   "MEMBER"   "MEMBERS"   x
                            <*> p_list p_themrole "THEMROLE" "THEMROLES" x
                            <*> p_list p_frame    "FRAME"    "FRAMES"    x
                            <*> x .: "ID"


p_vnclass :: Element -> Parser VNClass
p_vnclass x = VNClass <$> p_list p_member     "MEMBER"     "MEMBERS"    x
                      <*> p_list p_themrole   "THEMROLE"   "THEMROLES"  x
                      <*> p_list p_frame      "FRAME"      "FRAMES"     x
                      <*> p_list p_vnsubclass "VNSUBCLASS" "SUBCLASSES" x
                      <*> x .: "ID"


main :: IO ()
main = do
  let fp = "/scratch/wavewave/VerbNet/verbnet" </> "get-13.5.1.xml"
  txt <- TLIO.readFile fp
  case txt ^? html . allNamed (only "VNCLASS") of
    Nothing -> print "no "
    Just f -> print (p_vnclass f)
