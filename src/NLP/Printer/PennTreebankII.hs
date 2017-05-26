{-# LANGUAGE OverloadedStrings #-}

module NLP.Printer.PennTreebankII where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text as T  
--
import           NLP.Type.PennTreebankII

textprinter :: Int -> PennTree -> Text
textprinter n (PN _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PL _ txt) = T.replicate n " " <> txt

pennTreePrint :: Int -> PennTree -> Text
pennTreePrint n (PN t lst) = "\n" <> fmttag <> T.concat (map (pennTreePrint (n+2)) lst)
  where fmttag = T.replicate n " " <> T.take 4 (t <> "    ") <> " "
pennTreePrint _ (PL t txt) = T.take 4 (t <> "    ") <> " " <> txt


