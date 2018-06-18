{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module VerbNet.Parser where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
--
import           Text.Taggy.Lens.Util
--
import           VerbNet.Type


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
                          _ -> fail "p_selrestr_each"

p_synrestr :: Element -> Either String SynRestr
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
                                                                             _               -> fail "p_restrs"
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
                     "PREP" -> PREP <$> optional (y .: "value")
                     "LEX"  -> LEX <$> y .: "value"
                     z      -> fail ("p_syntax: p_each: " ++ show z)

p_arg :: Element -> Parser Arg
p_arg x = Arg <$> (x .: "type"  >>= \case ("Event" :: Text) -> pure Arg_Event
                                          "ThemRole"        -> pure Arg_ThemRole
                                          "VerbSpecific"    -> pure Arg_VerbSpecific
                                          "Constant"        -> pure Arg_Constant
                                          y                 -> fail ("p_arg: " ++ show y)
                  )
              <*> x .: "value"

p_pred :: Element -> Parser Pred
p_pred x = Pred <$> p_list p_arg "ARG" "ARGS" x
                <*> optional (x .: "bool")
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
