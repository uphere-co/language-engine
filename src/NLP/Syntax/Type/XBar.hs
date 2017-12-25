{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.PreAnalysis
, module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, module NLP.Syntax.Type.XBar
) where

import           Control.Lens                       ((^.),(^?),_1,_2,_Just,to)
import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe,maybeToList)
import           Data.Text                          (Text)
import qualified Data.Text                     as T
--
import           Data.Range                         (Range,isInside)
import           NLP.Type.PennTreebankII            (tokenWord)
--
import           NLP.Syntax.Type.PreAnalysis
import           NLP.Syntax.Type.XBar.Internal
import           NLP.Syntax.Type.XBar.TH



getTokens :: BitreeICP as -> Text
getTokens = T.intercalate " " . map (tokenWord.snd) . toList


tokensByRange :: PreAnalysis t -> Range -> [Text]
tokensByRange tagged rng = map (^._2._2) . filter (^._1.to (\i -> i `isInside` rng)) $ tagged^.lemmaList


determinerText :: PreAnalysis t -> HeadDP -> Maybe Text
determinerText tagged hdp = fmap (T.intercalate " " . tokensByRange tagged) (hdp^.hd_range)


headText :: PreAnalysis t -> NounP -> Text
headText tagged x = x^.headX.coidx_content.hn_range.to (T.intercalate " " . tokensByRange tagged)


specDPText :: PreAnalysis t -> SpecDP -> Text
specDPText tagged x = case x of
                        SpDP_Appos rng -> T.intercalate " " (tokensByRange tagged rng)
                        SpDP_Gen rng -> T.intercalate " " (tokensByRange tagged rng)


compVPToSpecTP :: CompVP -> SpecTP
compVPToSpecTP (CompVP_Unresolved x) = SpecTP_Unresolved x
compVPToSpecTP (CompVP_CP cp)        = SpecTP_Unresolved (cp^.maximalProjection)
compVPToSpecTP (CompVP_DP y)         = SpecTP_DP y
compVPToSpecTP (CompVP_PP y)         = case y^.complement of
                                         CompPP_DP dp -> SpecTP_DP dp
                                         CompPP_Gerund rng -> SpecTP_Unresolved rng
compVPToSpecTP (CompVP_AP ap)        = SpecTP_Unresolved (ap^.maximalProjection)  -- for the time being


specTPToCompVP :: SpecTP -> CompVP
specTPToCompVP (SpecTP_Unresolved x) = CompVP_Unresolved x
specTPToCompVP (SpecTP_DP x)         = CompVP_DP x


compVPToCPDPPP :: CompVP -> Maybe CPDPPP
compVPToCPDPPP (CompVP_Unresolved _) = Nothing
compVPToCPDPPP (CompVP_CP cp) = Just (CPCase cp)
compVPToCPDPPP (CompVP_DP dp) = Just (DPCase dp)
compVPToCPDPPP (CompVP_PP pp) = Just (PPCase pp)
compVPToCPDPPP (CompVP_AP pp) = Just (APCase pp)


headTextDP :: PreAnalysis t -> DetP -> Text
headTextDP tagged dp =
  case dp^.headX.hd_class of
    GenitiveClitic -> fromMaybe "" (fmap (headText tagged) (dp^.complement))
    _ -> T.intercalate " " (maybeToList (determinerText tagged (dp^.headX)) ++ maybeToList (fmap (headText tagged) (dp^.complement)))


headRangeDP :: DetP -> Maybe Range
headRangeDP dp =
  case dp^.headX.hd_class of
    GenitiveClitic -> dp^?complement._Just.headX.coidx_content.hn_range
    _ -> let mrng_det = dp^.headX.hd_range
             mrng_np = dp^?complement._Just.headX.coidx_content.hn_range
         in case (mrng_det,mrng_np) of
              (Just (b_det,_), Just (_,e_np)) -> Just (b_det,e_np)
              (Just rng_det  , Nothing      ) -> Just rng_det
              (Nothing       , Just rng_np  ) -> Just rng_np
              (Nothing       , Nothing      ) -> Nothing


compVPToHeadText :: PreAnalysis t -> CompVP -> Text
compVPToHeadText tagged (CompVP_Unresolved rng) = T.intercalate " " (tokensByRange tagged rng)
compVPToHeadText tagged (CompVP_CP cp)          = T.intercalate " " (tokensByRange tagged (cp^.maximalProjection))
compVPToHeadText tagged (CompVP_DP dp)          = headTextDP tagged dp
compVPToHeadText tagged (CompVP_PP pp)          = case pp^.complement of
                                                    CompPP_DP dp      -> headTextDP tagged dp
                                                    CompPP_Gerund rng -> T.intercalate " " (tokensByRange tagged rng)
compVPToHeadText tagged (CompVP_AP ap)          = T.intercalate " " (tokensByRange tagged (ap^.maximalProjection))


compVPToRange :: CompVP -> Range
compVPToRange = (\case SpecTP_Unresolved rng -> rng; SpecTP_DP dp -> dp^.maximalProjection) . compVPToSpecTP


compPPToRange :: CompPP -> Range
compPPToRange (CompPP_DP dp) = dp^.maximalProjection
compPPToRange (CompPP_Gerund rng) = rng


toRange :: CPDPPP -> Range
toRange (CPCase cp) = cp^.maximalProjection
toRange (DPCase dp) = dp^.maximalProjection
toRange (PPCase pp) = pp^.maximalProjection
toRange (APCase ap) = ap^.maximalProjection
