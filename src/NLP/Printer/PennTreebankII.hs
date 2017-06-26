{-# LANGUAGE OverloadedStrings #-}

module NLP.Printer.PennTreebankII where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text as T  
--
import           Data.Bitree
import           NLP.Type.PennTreebankII

fmttag :: Text -> Text
fmttag t = T.take 4 (t <> "    ")

indent :: Int -> Text
indent n = T.replicate n " "

prettyPrint :: Int -> PennTree -> Text
prettyPrint n (PN t lst) = "\n" <> indent n <> "(" <> fmttag t <> " " <> T.intercalate " " (map (prettyPrint (n+2)) lst) <> ")"
prettyPrint n (PL (".",_))  = "\n" <> indent n <> "(.     .)"
prettyPrint n (PL (",",_))  = "\n" <> indent n <> "(,     ,)"
prettyPrint n (PL ("``",_)) = "\n" <> indent n <> "(``   ``)"
prettyPrint n (PL ("''",_)) = "\n" <> indent n <> "(''   '')"
prettyPrint n (PL ("!", _)) = "\n" <> indent n <> "(!     !)"
prettyPrint n (PL ("?", _)) = "\n" <> indent n <> "(?     ?)"
prettyPrint _ (PL (t, txt)) = fmttag t <> " " <> txt


