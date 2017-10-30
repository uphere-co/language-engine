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

import           Control.Lens                       ((^.),_1,_2,to)
import           Data.Foldable                      (toList)
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


tokensByRange :: TaggedLemma t -> Range -> [Text]
tokensByRange tagged rng = map (^._2._2) . filter (^._1.to (\i -> i `isInside` rng)) $ tagged^.lemmaList


{- 
headRange :: DetP t -> Range
headRange x = x^.headX
-}

headText :: TaggedLemma t -> DetP t -> Text
headText tagged x = T.intercalate " " (tokensByRange tagged (x^.headX))

-- (x^.maximalProjection)


compVPToEither :: CompVP t -> Either (Zipper t) (DetP t)
compVPToEither (CompVP_Unresolved x) = Left  x
compVPToEither (CompVP_CP z)         = Left (z^.maximalProjection)
compVPToEither (CompVP_DP y)         = Right y
compVPToEither (CompVP_PP y)         = Right (y^.complement)




compVPToHeadText :: TaggedLemma as -> CompVP as -> Text
compVPToHeadText tagged (CompVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
compVPToHeadText tagged (CompVP_CP z)         = z^.maximalProjection.to (T.intercalate " " . map (tokenWord.snd) . toList . current)
compVPToHeadText tagged (CompVP_DP z)         = headText tagged z
compVPToHeadText tagged (CompVP_PP z)         = headText tagged (z^.complement)



compVPToRange :: CompVP as -> Range
compVPToRange = either (getRange.current) (\dp->dp^.maximalProjection) . compVPToEither

--  (\dp->dp^.maximalProjection.to (getRange.current)) . compVPToEither 
