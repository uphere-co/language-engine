{-# LANGUAGE OverloadedStrings #-}

module NLP.Printer.PennTreebankII where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text as T  
--
import           NLP.Type.PennTreebankII

{- 
textprinter :: Int -> PennTree -> Text
textprinter n (PN _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PL _ txt) = T.replicate n " " <> txt
-}

fmttag t = T.take 4 (t <> "    ")

indent n = T.replicate n " "


prettyPrint :: Int -> PennTree -> Text
prettyPrint n (PN t lst) = "\n" <> indent n <> "(" <> fmttag t <> " " <> T.intercalate " " (map (prettyPrint (n+2)) lst) <> ")"
prettyPrint n (PL "." _)  = "\n" <> indent n <> "(.     .)"
prettyPrint n (PL "," _)  = "\n" <> indent n <> "(,     ,)"
prettyPrint n (PL "``" _) = "\n" <> indent n <> "(``   ``)"
prettyPrint n (PL "''" _) = "\n" <> indent n <> "(''   '')"
prettyPrint n (PL "!" _)  = "\n" <> indent n <> "(!     !)"
prettyPrint n (PL "?" _)  = "\n" <> indent n <> "(?     ?)"
prettyPrint _ (PL t txt) = fmttag t <> " " <> txt


