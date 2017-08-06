{-# LANGUAGE OverloadedStrings #-}

-- the functions in this module will be relocated to a more common package like textview

module OntoNotes.App.Util where

import           Data.Text                (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T.IO
--
import           Text.Annotation.Type
import           Text.Annotation.Util.Doc
import           Text.Annotation.View

type SentIdx = Int
type CharIdx = Int
type BeginEnd = (CharIdx,CharIdx)
type TagPos a = (CharIdx,CharIdx,a)
type SentItem = (SentIdx,BeginEnd,Text)


addText :: Text -> (SentIdx,BeginEnd) -> SentItem
addText txt (n,(b,e)) = (n,(b,e),slice (b-1) e txt)


addTag :: [TagPos a] -> SentItem -> (SentItem,[TagPos a])
addTag lst i@(_,(b,e),_) = (i,filter check lst)
  where check (b',e',_) = b' >= b && e' <= e


underlineText :: BeginEnd -> Text -> [TagPos a] -> IO ()
underlineText (b0,_e0) txt lst = do
  let f n (b,e,_) = (n,b-b0+1,e-b0+1)
      tagged = zipWith f [1..] lst
      ann = AnnotText (tagText tagged txt)
      xss = lineSplitAnnot Nothing 80 ann
      formatInt = T.pack . show
      ls = do xs <- xss
              x <- xs
              underlineAnnotWithLabel (fmap formatInt) x
      result = T.intercalate "\n" ls
  T.IO.putStrLn result

