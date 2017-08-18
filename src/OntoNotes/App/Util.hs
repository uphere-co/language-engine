{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell     #-}

-- the functions in this module will be relocated to a more common package like textview

module OntoNotes.App.Util where

import           Control.Lens
import           Data.Maybe               (fromJust)
import           Data.Text                (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T.IO
import           Text.ProtocolBuffers.Basic       (Utf8)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple.Convert                           (cutf8)
import           CoreNLP.Simple.Type.Simplified
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

underlineText :: (a -> Text) -> BeginEnd -> Text -> [TagPos a] -> [Text]
underlineText lblf (b0,_e0) txt taglst =
  let adjf (b,e,z) = (z,b-b0+1,e-b0+1)
      -- tagged = zipWith f [1..] lst
      ann = AnnotText (tagText (map adjf taglst) txt)
      xss = lineSplitAnnot Nothing 80 ann
      -- formatInt = T.pack . show
      ls = do xs <- xss
              x <- xs
              underlineAnnotWithLabel (fmap lblf) x
  in ls
  --     result = T.intercalate "\n" ls
  -- T.IO.putStrLn result


getSentenceOffsets :: [S.Sentence] -> [(SentIdx,BeginEnd)]
getSentenceOffsets psents =
  zip ([1..] :: [Int]) $ flip map psents $ \s ->
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (fromIntegral b+1,fromIntegral e)


addSUTime :: [SentItem] -> T.ListTimex
          -> [(SentItem,[TagPos (Maybe Text)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
      cvt = map (\(x,xs) -> (x,map(\(i,j,z) -> (i,j,(fmap cutf8 z))) xs))
  in (cvt . filter (not.null.(^._2)) . map (addTag (map f (tmxs^..T.timexes.traverse)))) sents


