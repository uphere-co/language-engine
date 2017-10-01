{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, getTokens
, tokensByRange
, headRange
, headText
, compVPToEither
, compVPToHeadText
) where

import           Control.Lens                       ((^.),(^?),_1,_2,_Just,to)
-- import           Control.Lens                       ((^.),_1,_2,to)
import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                     as T
--
import           Data.BitreeZipper                  (current)
import           Data.Range                         (Range,isInside)
import           NLP.Type.PennTreebankII            (ALeaf,tokenWord)
--
import           NLP.Syntax.Type.XBar.Internal
import           NLP.Syntax.Type.XBar.TH


getTokens :: BitreeICP as -> Text
getTokens = T.intercalate " " . map (tokenWord.snd) . toList


tokensByRange :: (Foldable t) => Range -> t (Int, ALeaf a) -> [Text]
tokensByRange rng = map snd . filter (^._1.to (\i -> i `isInside` rng)) . map (\(i,x)->(i,tokenWord x)) . toList


headRange :: DetP t -> Range
headRange x = x^.headX._2


headText :: DetP t -> Text
headText x = (T.intercalate " " . tokensByRange (headRange x) . current) (x^.maximalProjection)


compVPToEither :: CompVP t -> Either (Zipper t) (DetP t)
compVPToEither (CompVP_Unresolved x) = Left  x
compVPToEither (CompVP_CP z)         = Left (fromMaybe (error "compVPToEither CP") (z^.maximalProjection))  -- this case should not happen.
compVPToEither (CompVP_DP y)         = Right y
compVPToEither (CompVP_PP y)    = Right (y^.complement)




compVPToHeadText :: CompVP as -> Text
compVPToHeadText (CompVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
compVPToHeadText (CompVP_CP z) = fromMaybe "" (z^?maximalProjection._Just.to (T.intercalate " " . map (tokenWord.snd) . toList . current))
compVPToHeadText (CompVP_DP z) = headText z
compVPToHeadText (CompVP_PP z) = headText (z^.complement)
