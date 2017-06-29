{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module VerbNet where

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
                           , _synrestrs_logic :: Maybe AndOr
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


data ArgType = Arg_Event
             | Arg_ThemRole
             | Arg_VerbSpecific
             | Arg_Constant
             deriving (Show)
                      

data Arg = Arg { _arg_type :: ArgType
               , _arg_value :: Text
               }
         deriving Show

makeLenses ''Arg                  

data Pred = Pred { _pred_args :: [Arg]
                 , _pred_value :: Text
                 }
          deriving Show

makeLenses ''Pred                   



data Frame = Frame { _frame_description :: Description
                   , _frame_examples    :: [Text]
                   , _frame_syntax      :: [Syntax]
                   , _frame_semantics   :: [Pred]
                   }
           deriving (Show)


makeLenses ''Frame                    

data Member = Member { _member_name     :: Text
                     , _member_wn       :: Text
                     , _member_grouping :: Maybe Text
                     }
            deriving Show

makeLenses ''Member                     

data ThemRole = ThemRole { _themrole_selrestrs :: SelRestrs
                         , _themrole_type :: Text
                         }
            deriving Show

makeLenses ''ThemRole                     


data VNSubclass = VNSubclass { _vnsubclass_members   :: [Member]
                             , _vnsubclass_themroles :: [ThemRole]
                             , _vnsubclass_frames    :: [Frame]
                             , _vnsubclass_subclasses :: [VNSubclass]
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
p_themrole x = ThemRole <$> (p_selrestrs =<< getOnly1 x "SELRESTRS")
                        <*> x .: "type"

p_description :: Element -> Parser Description
p_description x = Description <$> x .: "primary"
                              <*> optional (x .: "secondary")
                              <*> x .: "descriptionNumber"
                              <*> x .: "xtag"


p_selrestrs :: Element -> Parser SelRestrs
p_selrestrs x = SelRestrs <$> traverse p_selrestr_each (x^..elements)
  where
    p_selrestr_each y = case y^.name of
                          "SELRESTR" -> Left <$> (SelRestr <$> y .: "Value" <*> y .: "type")
                          "SELRESTRS" -> Right <$> p_selrestrs y
  
p_synrestr x = SynRestr <$> x .: "Value"
                        <*> x .: "type"
                        
p_restrs :: Element -> Parser (Either SynRestrs SelRestrs)
p_restrs x = case x ^.. elements of
               []    -> fail "p_restrs: no element"
               (y:_) -> case y^.name of
                          "SYNRESTRS" -> Left <$> (SynRestrs <$> traverse p_synrestr (y^..elements)
                                                             <*> optional (x .: "logic" >>= \case
                                                                             ("and" :: Text) -> pure And
                                                                             "or"            -> pure Or
                                                                          )
                                                  )
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

p_arg :: Element -> Parser Arg
p_arg x = Arg <$> (x .: "type"  >>= \case ("Event" :: Text) -> pure Arg_Event
                                          "ThemRole"        -> pure Arg_ThemRole
                                          "VerbSpecific"    -> pure Arg_VerbSpecific
                                          "Constant"        -> pure Arg_Constant
                  )
              <*> x .: "value"

p_pred :: Element -> Parser Pred
p_pred x = Pred <$> p_list p_arg "ARG" "ARGS" x
                <*> x .: "value"
          
p_frame :: Element -> Parser Frame
p_frame x = Frame <$> (p_description =<< getOnly1 x "DESCRIPTION")
                  <*> p_list (pure . (^.contents)) "EXAMPLE" "EXAMPLES" x
                  <*> (p_syntax =<< getOnly1 x "SYNTAX")
                  <*> p_list p_pred "PRED" "SEMANTICS" x

                  


p_vnsubclass :: Element -> Parser VNSubclass
p_vnsubclass x = VNSubclass <$> p_list p_member   "MEMBER"   "MEMBERS"   x
                            <*> p_list p_themrole "THEMROLE" "THEMROLES" x
                            <*> p_list p_frame    "FRAME"    "FRAMES"    x
                            <*> (p_list p_vnsubclass "VNSUBCLASS" "SUBCLASSES" x <|> pure [])
                            <*> x .: "ID"


p_vnclass :: Element -> Parser VNClass
p_vnclass x = VNClass <$> p_list p_member     "MEMBER"     "MEMBERS"    x
                      <*> p_list p_themrole   "THEMROLE"   "THEMROLES"  x
                      <*> p_list p_frame      "FRAME"      "FRAMES"     x
                      <*> p_list p_vnsubclass "VNSUBCLASS" "SUBCLASSES" x
                      <*> x .: "ID"


main :: IO ()
main = do
  let dir= "/scratch/wavewave/VerbNet/verbnet"
  {- cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts) -}
  let fs = [ "get-13.5.1.xml" ]
  flip mapM_ fs $ \f -> do
    let fp = dir  </> f
    txt <- TLIO.readFile fp
    case txt ^? html . allNamed (only "VNCLASS") of
      Nothing -> error "nothing"
      Just f -> case p_vnclass f of
                  Left err -> error err
                  Right c  -> print c
