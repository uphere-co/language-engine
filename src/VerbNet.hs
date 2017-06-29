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


data SelRestr = SelRestr { _selrestr_value :: Text
                         , _selrestr_type :: Text }
              deriving Show

makeLenses ''SelRestr

data SelRestrs = SelRestrs { _selrestrs_elements :: [Either SelRestr SelRestrs] }
               deriving Show

makeLenses ''SelRestrs


data SynRestr = SynRestr { _synrestr_value :: Text
                         , _synrestr_type :: Text
                         }
              deriving Show

makeLenses ''SynRestr

data AndOr = And | Or
           deriving Show

data SynRestrs = SynRestrs { _synrestrs_elements :: [SynRestr]
                           -- , _synrestrs_logic :: Maybe AndOr
                           }
               deriving Show

makeLenses ''SynRestrs



data Syntax = NP   { _np_restrs :: Either SynRestrs SelRestrs
                   , _np_value :: Text
                   }
            | VERB
            | ADJ
            | ADV
            | PREP { _prep_value :: Text }
            | LEX  { _lex_value :: Text }
            deriving Show

data Frame = Frame { _frame_description :: Description
                   , _frame_examples    :: [Text]
                   , _frame_syntax      :: [Syntax]
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


p_selrestrs :: Element -> Parser SelRestrs
p_selrestrs x = SelRestrs <$> traverse p_selrestr_each (x^..elements)
  where
    p_selrestr_each y = case y^.name of
                          "SYNRESTR" -> Left <$> (SelRestr <$> y .: "Value" <*> y .: "type")
                          "SYNRESTRS" -> Right <$> p_selrestrs y
  
p_synrestr x = SynRestr <$> x .: "Value"
                        <*> x .: "type"
                        
p_restrs :: Element -> Parser (Either SynRestrs SelRestrs)
p_restrs x = case x ^.. elements of
               []    -> fail "p_restrs: no element"
               (y:_) -> case y^.name of
                          "SYNRESTRS" -> Left . SynRestrs <$> traverse p_synrestr (y^..elements)
                          "SELRESTRS" -> Right <$> p_selrestrs y 
                          z           -> fail ("p_restrs: " ++ show z)

p_syntax :: Element -> Parser [Syntax]
p_syntax x = let ys = x^..elements
             in traverse p_each ys
  where p_each y = case y^.name of
                     "NP"   -> NP <$> p_restrs y <*> y .: "value"
                     "VERB" -> pure VERB
                     "ADJ"  -> pure ADJ
                     "ADV"  -> pure ADV
                     "PREP" -> PREP <$> y .: "value"
                     "LEX"  -> LEX <$> y .: "value"
                     x      -> fail ("p_syntax: p_each: " ++ show x)
          
p_frame :: Element -> Parser Frame
p_frame x = Frame <$> (p_description =<< getOnly1 x "DESCRIPTION")
                  <*> p_list (pure . (^.contents)) "EXAMPLE" "EXAMPLES" x
                  <*> (p_syntax =<< getOnly1 x "SYNTAX")

                  


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
