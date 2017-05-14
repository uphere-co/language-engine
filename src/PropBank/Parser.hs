{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropBank.Parser where

import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Taggy.Lens
--
import           PropBank.Type

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
                 (x:_) -> Right x


type Parser = Either String

p_frameset :: Element -> Parser FrameSet
p_frameset x = FrameSet <$> mapM p_note (getOnly x "note")
                        <*> mapM p_predicate (getOnly x "predicate")

p_predicate :: Element -> Parser Predicate
p_predicate x = Pred <$> mapM p_note (getOnly x "note")
                     <*> mapM p_roleset (getOnly x "roleset")
                     <*> x .: "lemma"

p_roleset :: Element -> Parser RoleSet
p_roleset x = RoleSet <$> mapM p_note (getOnly x "note")
                      <*> (p_roles =<< getOnly1 x "roles")
                      <*> mapM p_example (getOnly x "example")
                      <*> x .: "id"
                      <*> x .: "name"
                      <*> x .: "source"
                      <*> x .: "vncls"
                      <*> x .: "roleset"
                      <*> x .: "framenet"

p_example :: Element -> Parser Example
p_example x = Example <$> inf
                      <*> mapM p_note (getOnly x "note")
                      <*> ((^.contents) <$> getOnly1 x "text")
                      <*> mapM p_arg (getOnly x "arg")
                      <*> mapM p_rel (getOnly x "rel")
                      <*> x .: "name"
                      <*> x .: "type"
                      <*> x .: "src"
  where inf = case getOnly x "inflection" of
                [] -> Right Nothing
                (x:_) -> Just <$> p_inflection x 

p_inflection :: Element -> Parser Inflection 
p_inflection x = Inflection <$> (p_person =<< x .: "person")
                            <*> (p_tense  =<< x .: "tense" )
                            <*> (p_aspect =<< x .: "aspect")
                            <*> (p_voice  =<< x .: "voice" )
                            <*> (p_form   =<< x .: "form"  )

p_arg :: Element -> Parser Arg
p_arg x = Arg <$> pure (x^.contents)
              <*> x .: "n"
              <*> x .: "h"
              <*> x .: "f"

p_rel :: Element -> Parser Rel
p_rel x = Rel <$> pure (x^.contents)
              <*> x .: "n"
              <*> x .: "h"
              <*> x .: "f"


p_note :: Element -> Parser Note
p_note x = Note <$>  pure (x ^. element.contents)

p_roles :: Element -> Parser Roles
p_roles x = Roles <$> mapM p_note (getOnly x "note")
                  <*> mapM p_role (getOnly x "role") 

p_role :: Element -> Parser Role
p_role x = Role <$> mapM p_vnrole (getOnly x "vnrole")
                <*> x .: "n"
                <*> x .: "f"
                <*> x .: "source"
                <*> x .: "descr"

p_vnrole :: Element -> Parser VNRole
p_vnrole x = VNRole <$> x .: "vncls"
                    <*> (p_vntheta =<< (x .: "vntheta"))

p_vntheta :: Text -> Parser VNTheta
p_vntheta "Actor1"      = Right Actor1
p_vntheta "Actor2"      = Right Actor2
p_vntheta "Agent"       = Right Agent
p_vntheta "Asset"       = Right Asset
p_vntheta "Attribute"   = Right Attribute
p_vntheta "Beneficiary" = Right Beneficiary
p_vntheta "Cause"       = Right Cause
p_vntheta "Destination" = Right Destination
p_vntheta "Experiencer" = Right Experiencer
p_vntheta "Extent"      = Right Extent
p_vntheta "Instrument"  = Right Instrument
p_vntheta "Location"    = Right Location
p_vntheta "Material"    = Right Material
p_vntheta "Patient"     = Right Patient
p_vntheta "Patient1"    = Right Patient1
p_vntheta "Patient2"    = Right Patient2
p_vntheta "Predicate"   = Right Predicate
p_vntheta "Product"     = Right Product
p_vntheta "Recipient"   = Right Recipient
p_vntheta "Source"      = Right Source
p_vntheta "Stimulus"    = Right Stimulus
p_vntheta "Theme"       = Right Theme
p_vntheta "Theme1"      = Right Theme1
p_vntheta "Theme2"      = Right Theme2
p_vntheta "Time"        = Right Time
p_vntheta "Topic"       = Right Topic
p_vntheta x             = Left ("VNTheta:" ++ (T.unpack x))

p_person :: Text -> Parser (Maybe Person)
p_person "third" = Right (Just Third)
p_person "other" = Right (Just Other)
p_person "ns"    = Right Nothing
p_person x       = Left ("person:" ++ T.unpack x)

p_tense :: Text -> Parser (Maybe Tense)
p_tense "present" = Right (Just Present)
p_tense "past"    = Right (Just Past)
p_tense "future"  = Right (Just Future)
p_tense "ns"      = Right Nothing
p_tense x         = Left ("tense:" ++ T.unpack x)

p_aspect :: Text -> Parser (Maybe Aspect)
p_aspect "perfect"     = Right (Just Perfect)
p_aspect "progressive" = Right (Just Progressive)
p_aspect "both"        = Right (Just Both)
p_aspect "ns"          = Right Nothing
p_aspect x             = Left ("aspect:" ++ T.unpack x)

p_voice :: Text -> Parser (Maybe Voice)
p_voice "active"  = Right (Just Active)
p_voice "passive" = Right (Just Passive)
p_voice "ns"      = Right Nothing
p_voice x         = Left ("voice:" ++ T.unpack x)

p_form :: Text -> Parser (Maybe Form)
p_form "infinitive" = Right (Just Infinitive)
p_form "gerund"     = Right (Just Gerund)
p_form "participle" = Right (Just Participle)
p_form "full"       = Right (Just Full)
p_form "ns"         = Right Nothing
p_form x            = Left ("form:" ++ T.unpack x)
