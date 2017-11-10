{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, module NLP.Syntax.Type.XBar
) where

import           Control.Lens                       ((^.),(^?),_1,_2,_Just,to)
import           Data.Foldable                      (toList)
import           Data.Maybe                         (catMaybes,fromMaybe,maybeToList)
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

determinerText :: TaggedLemma t -> HeadDP -> Maybe Text
determinerText tagged hdp = fmap (T.intercalate " " . tokensByRange tagged) (hdp^.hd_range)

{-   case hdp^.hd_class of
    (Pronoun ptyp) -> Just $ case ptyp of
                               P_I    -> "I"
                               P_You  -> "you"
                               P_He   -> "he"
                               P_She  -> "she"
                               P_It   -> "it"
                               P_They -> "they"
    _              -> Nothing
-}


headText :: TaggedLemma t -> NounP t -> Text
headText tagged x = x^.headX.to (T.intercalate " " . tokensByRange tagged)



compVPToEither :: CompVP t -> Either (Zipper t) (DetP t)
compVPToEither (CompVP_Unresolved x) = Left  x
compVPToEither (CompVP_CP cp)        = Left (cp^.maximalProjection)
compVPToEither (CompVP_DP y)         = Right y
compVPToEither (CompVP_PP y)         = case y^.complement of
                                         CompPP_DP dp -> Right dp
                                         CompPP_Gerund z -> Left z


headTextDP :: TaggedLemma t -> DetP t -> Text
headTextDP tagged dp = T.intercalate " " (maybeToList (determinerText tagged (dp^.headX)) ++ maybeToList (fmap (headText tagged) (dp^.complement)))


compVPToHeadText :: TaggedLemma t -> CompVP t -> Text
compVPToHeadText _tagged (CompVP_Unresolved z) = (T.intercalate " " . map (tokenWord.snd) . toList . current) z
compVPToHeadText _tagged (CompVP_CP cp)        = cp^.maximalProjection.to (T.intercalate " " . map (tokenWord.snd) . toList . current)
compVPToHeadText tagged  (CompVP_DP dp)        = headTextDP tagged dp
compVPToHeadText tagged  (CompVP_PP pp)        = case pp^.complement of
                                                   CompPP_DP dp -> headTextDP tagged dp
                                                   CompPP_Gerund z -> (T.intercalate " " . map (tokenWord.snd) . toList . current) z



compVPToRange :: CompVP t -> Range
compVPToRange = either (getRange.current) (\dp->dp^.maximalProjection) . compVPToEither


compDPToRange :: CompDP t -> Range
compDPToRange (CompDP_Unresolved rng) = rng
compDPToRange (CompDP_CP cp) = cp^.maximalProjection.to current.to getRange
compDPToRange (CompDP_PP pp) = pp^.maximalProjection


compPPToRange :: CompPP t -> Range
compPPToRange (CompPP_DP dp) = fromMaybe (dp^.maximalProjection) (dp^?complement._Just.headX)
compPPToRange (CompPP_Gerund z) = getRange (current z)
