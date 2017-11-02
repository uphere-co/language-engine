{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, module NLP.Syntax.Type.XBar
) where

import           Control.Lens                       ((^.),_1,_2,to)
import           Data.Foldable                      (toList)
import           Data.Text                          (Text)
import qualified Data.Text                     as T
--
import           Data.BitreeZipper                  (current)
import           Data.Range                         (Range,isInside)
import           NLP.Type.PennTreebankII            (tokenWord,getRange)
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
compVPToEither (CompVP_CP cp)        = Left (cp^.maximalProjection)
compVPToEither (CompVP_DP y)         = Right y
compVPToEither (CompVP_PP y)         = case y^.complement of
                                         CompPP_DP dp -> Right dp
                                         CompPP_Gerund z -> Left z




compVPToHeadText :: TaggedLemma as -> CompVP as -> Text
compVPToHeadText _tagged (CompVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
compVPToHeadText _tagged (CompVP_CP cp)        = cp^.maximalProjection.to (T.intercalate " " . map (tokenWord.snd) . toList . current)
compVPToHeadText tagged  (CompVP_DP dp)        = headText tagged dp
compVPToHeadText tagged  (CompVP_PP pp)        = case pp^.complement of
                                                   CompPP_DP dp -> headText tagged dp
                                                   CompPP_Gerund z -> (T.intercalate " " . map (tokenWord.snd) . toList . current) z



compVPToRange :: CompVP t -> Range
compVPToRange = either (getRange.current) (\dp->dp^.maximalProjection) . compVPToEither


compPPToRange :: CompPP t -> Range
compPPToRange (CompPP_DP dp) = dp^.headX
compPPToRange (CompPP_Gerund z) = getRange (current z)
