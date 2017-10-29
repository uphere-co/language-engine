{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, getTokens
, tokensByRange
-- , headRange
, headText
, compVPToEither
, compVPToHeadText
, compVPToRange
) where

import           Control.Lens                       ((^.),(^..),(^?),_1,_2,_Just,_Right,to)
import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                     as T
--
import           Data.BitreeZipper                  (current)
import           Data.Range                         (Range,isInside)
import           NLP.Type.PennTreebankII            (ALeaf,tokenWord,getRange)
--
import           NLP.Syntax.Type.XBar.Internal
import           NLP.Syntax.Type.XBar.TH



getTokens :: BitreeICP as -> Text
getTokens = T.intercalate " " . map (tokenWord.snd) . toList


tokensByRange :: (Foldable t) => Range -> t (Int, ALeaf a) -> [Text]
tokensByRange rng = map snd . filter (^._1.to (\i -> i `isInside` rng)) . map (\(i,x)->(i,tokenWord x)) . toList


{- 
headRange :: DetP t -> Range
headRange x = x^.headX
-}

headText :: DetP t -> Text
headText x = (x^.maximalProjection.original.to (T.intercalate " " . tokensByRange (x^.headX) . current))

-- (x^.maximalProjection)


compVPToEither :: CompVP t -> Either (Zipper t) (DetP t)
compVPToEither (CompVP_Unresolved x) = Left  x
compVPToEither (CompVP_CP z)         = Left (z^.maximalProjection)
compVPToEither (CompVP_DP y)         = Right y
compVPToEither (CompVP_PP y)         = Right (y^.complement)




compVPToHeadText :: CompVP as -> Text
compVPToHeadText (CompVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
compVPToHeadText (CompVP_CP z)         = z^.maximalProjection.to (T.intercalate " " . map (tokenWord.snd) . toList . current)
compVPToHeadText (CompVP_DP z)         = headText z
compVPToHeadText (CompVP_PP z)         = headText (z^.complement)



compVPToRange :: CompVP as -> Range
compVPToRange = either (getRange.current) (\dp->dp^.maximalProjection.maximal) . compVPToEither

--  (\dp->dp^.maximalProjection.to (getRange.current)) . compVPToEither 
