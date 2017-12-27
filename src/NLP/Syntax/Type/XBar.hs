{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module NLP.Syntax.Type.XBar
( module NLP.Syntax.Type.PreAnalysis
, module NLP.Syntax.Type.XBar.Internal
, module NLP.Syntax.Type.XBar.TH
, module NLP.Syntax.Type.XBar
) where

import           Control.Lens                       ((^.),(^?),(.~),_1,_2,_Just,to)
import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe,mapMaybe,maybeToList)
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


headText :: PreAnalysis t -> NounP 'PH0 -> Text
headText tagged x = x^.headX.coidx_content.hn_range.to (T.intercalate " " . tokensByRange tagged)


specDPText :: PreAnalysis t -> SpecDP -> Text
specDPText tagged x = case x of
                        SpDP_Appos rng -> T.intercalate " " (tokensByRange tagged rng)
                        SpDP_Gen rng -> T.intercalate " " (tokensByRange tagged rng)


compVPToSpecTP :: SPhase p -> CompVP p -> Either Range (SpecTP p)
compVPToSpecTP SPH0 (CompVP_CP cp) = Left (cp^.maximalProjection)
compVPToSpecTP SPH0 (CompVP_DP y)  = Right (SpecTP_DP y)
compVPToSpecTP SPH0 (CompVP_PP y)  = case y^.complement of
                                       CompPP_DP dp -> Right (SpecTP_DP dp)
                                       CompPP_Gerund rng -> Left rng
compVPToSpecTP SPH0 (CompVP_AP ap) = Left (ap^.maximalProjection)  -- for the time being
compVPToSpecTP SPH1 (CompVP_CP cp) = Left cp
compVPToSpecTP SPH1 (CompVP_DP y)  = Right (SpecTP_DP y)
compVPToSpecTP SPH1 (CompVP_PP y)  = Left y   -- for the time being
                                     -- case y^.complement of
                                     --   CompPP_DP dp -> Right (SpecTP_DP dp)
                                     --  CompPP_Gerund rng -> Left rng
compVPToSpecTP SPH1 (CompVP_AP ap) = Left ap -- Left (ap^.maximalProjection)  -- for the time being



{-
  = case s of
                                           SPH0 -> Left (cp^.maximalProjection)
                                           SPH1 -> Left cp -}


specTPToCompVP :: SpecTP 'PH0 -> CompVP 'PH0
specTPToCompVP (SpecTP_DP x)         = CompVP_DP x


compVPToCPDPPP :: CompVP 'PH0 -> CPDPPP 'PH0
-- compVPToCPDPPP (CompVP_Unresolved _) = Nothing
compVPToCPDPPP (CompVP_CP cp) = CPCase cp
compVPToCPDPPP (CompVP_DP dp) = DPCase dp
compVPToCPDPPP (CompVP_PP pp) = PPCase pp
compVPToCPDPPP (CompVP_AP pp) = APCase pp


headTextDP :: PreAnalysis t -> DetP 'PH0 -> Text
headTextDP tagged dp =
  case dp^.headX.hd_class of
    GenitiveClitic -> fromMaybe "" (fmap (headText tagged) (dp^.complement))
    _ -> T.intercalate " " (maybeToList (determinerText tagged (dp^.headX)) ++ maybeToList (fmap (headText tagged) (dp^.complement)))


headRangeDP :: DetP 'PH0 -> Maybe Range
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


compVPToHeadText :: PreAnalysis t -> CompVP 'PH0 -> Text
-- compVPToHeadText tagged (CompVP_Unresolved rng) = T.intercalate " " (tokensByRange tagged rng)
compVPToHeadText tagged (CompVP_CP cp)          = T.intercalate " " (tokensByRange tagged (cp^.maximalProjection))
compVPToHeadText tagged (CompVP_DP dp)          = headTextDP tagged dp
compVPToHeadText tagged (CompVP_PP pp)          = case pp^.complement of
                                                    CompPP_DP dp      -> headTextDP tagged dp
                                                    CompPP_Gerund rng -> T.intercalate " " (tokensByRange tagged rng)
compVPToHeadText tagged (CompVP_AP ap)          = T.intercalate " " (tokensByRange tagged (ap^.maximalProjection))


compVPToRange :: SPhase p -> CompVP p -> Range
compVPToRange SPH0 (CompVP_CP cp) = cp^.maximalProjection
compVPToRange SPH0 (CompVP_DP dp) = dp^.maximalProjection
compVPToRange SPH0 (CompVP_PP pp) = pp^.maximalProjection
compVPToRange SPH0 (CompVP_AP ap) = ap^.maximalProjection
compVPToRange SPH1 (CompVP_CP cp) = cp
compVPToRange SPH1 (CompVP_DP dp) = dp
compVPToRange SPH1 (CompVP_PP pp) = pp
compVPToRange SPH1 (CompVP_AP ap) = ap


compPPToRange :: CompPP 'PH0 -> Range
compPPToRange (CompPP_DP dp) = dp^.maximalProjection
compPPToRange (CompPP_Gerund rng) = rng


toRange :: CPDPPP 'PH0 -> Range
toRange (CPCase cp) = cp^.maximalProjection
toRange (DPCase dp) = dp^.maximalProjection
toRange (PPCase pp) = pp^.maximalProjection
toRange (APCase ap) = ap^.maximalProjection



mkSpecTPPH1 :: SpecTP 'PH0 -> SpecTP 'PH1
mkSpecTPPH1 (SpecTP_DP dp) = SpecTP_DP (dp^.maximalProjection) -- (mkDPPH1 dp)


mkAdjunctCPPH1 :: AdjunctCP 'PH0 -> AdjunctCP 'PH1
mkAdjunctCPPH1 (AdjunctCP_CP cp) = AdjunctCP_CP (cp^.maximalProjection) -- (mkCPPH1 cp)




mkCompPPPH1 (CompPP_DP dp) = CompPP_DP (dp^.maximalProjection) -- (mkDPPH1 dp)
mkCompPPPH1 (CompPP_Gerund rng) = CompPP_Gerund rng


mkAdjunctVPPH1 (AdjunctVP_PP pp) = AdjunctVP_PP (pp^.maximalProjection) -- (mkPPPH1 pp)


mkVerbPPH1 :: VerbP 'PH0 -> VerbP 'PH1
mkVerbPPH1 vp = XP { _headX = _headX vp
                   , _maximalProjection = _maximalProjection vp
                   , _specifier = _specifier vp
                   , _adjunct = mapMaybe (either (const Nothing) (Just . mkAdjunctVPPH1)) (_adjunct vp)
                   , _complement = flip map (_complement vp) $ \t ->
                                     case t^.coidx_content of
                                       Left tr         -> (coidx_content .~ Left tr) t
                                       Right (Right p) -> (coidx_content .~ Right (mkCompVPPH1 p)) t
                                       Right (Left _)  -> (coidx_content .~ Left NULL) t
                   }

mkTPPH1 :: TP 'PH0 -> TP 'PH1
mkTPPH1 tp = XP { _headX = _headX tp
                , _maximalProjection = _maximalProjection tp
                , _specifier = let t = _specifier tp
                               in case t^.coidx_content of
                                    Left tr         -> (coidx_content .~ Left tr) t
                                    Right (Right p) -> (coidx_content .~ Right (mkSpecTPPH1 p)) t
                                    Right (Left _)  -> (coidx_content .~ Left NULL) t
                , _adjunct = _adjunct tp
                , _complement = mkVerbPPH1 (_complement tp)
                }




mkCompVPPH1 (CompVP_CP cp) = CompVP_CP (cp^.maximalProjection) -- (mkCPPH1 cp)
mkCompVPPH1 (CompVP_DP dp) = CompVP_DP (dp^.maximalProjection) -- (mkDPPH1 dp)
mkCompVPPH1 (CompVP_PP pp) = CompVP_PP (pp^.maximalProjection) -- (mkPPPH1 pp)
mkCompVPPH1 (CompVP_AP ap) = CompVP_AP (ap^.maximalProjection) -- (mkAPPH1 ap)


mkSpecCPPH1 :: SpecCP 'PH0 -> Maybe (SpecCP 'PH1)
mkSpecCPPH1 SpecCP_WHPHI    = Just SpecCP_WHPHI
mkSpecCPPH1 (SpecCP_WH rng) = Just (SpecCP_WH rng)
mkSpecCPPH1 (SpecCP_Topic t) = case t^.coidx_content of
                                 Left tr         -> Just (SpecCP_Topic ((coidx_content .~ Left tr) t))
                                 Right (Right p) -> Just (SpecCP_Topic ((coidx_content .~ Right (mkCompVPPH1 p)) t))
                                 Right (Left _)  -> Nothing






mkNPPH1 np = XP { _headX = _headX np
                , _maximalProjection = _maximalProjection np
                , _specifier = _specifier np
                , _adjunct = _adjunct np
                , _complement = _complement np
                }

mkDPPH1 dp = XP { _headX = _headX dp
                , _maximalProjection = _maximalProjection dp
                , _specifier = _specifier dp
                , _adjunct = _adjunct dp
                , _complement = fmap mkNPPH1 (_complement dp)
                }


mkAPPH1 :: AP 'PH0 -> AP 'PH1
mkAPPH1 ap = XP { _headX = _headX ap
                , _maximalProjection = _maximalProjection ap
                , _specifier = _specifier ap
                , _adjunct = _adjunct ap
                , _complement = _complement ap
                }



mkPPPH1 :: PP 'PH0 -> PP 'PH1
mkPPPH1 pp = XP { _headX = _headX pp
                , _maximalProjection = _maximalProjection pp
                , _specifier = _specifier pp
                , _adjunct = _adjunct pp
                , _complement = mkCompPPPH1 (_complement pp)
                }



mkCPPH1 :: CP 'PH0 -> CP 'PH1
mkCPPH1 cp = XP { _headX = _headX cp
                , _maximalProjection = _maximalProjection cp
                , _specifier = mkSpecCPPH1 =<< _specifier cp
                , _adjunct = mapMaybe (either (const Nothing) (Just. mkAdjunctCPPH1)) (_adjunct cp)
                , _complement = mkTPPH1 (_complement cp)
                }


mkCPDPPPPH1 (CPCase cp) = CPCase (mkCPPH1 cp)
mkCPDPPPPH1 (DPCase dp) = DPCase (mkDPPH1 dp)
mkCPDPPPPH1 (PPCase pp) = PPCase (mkPPPH1 pp)
mkCPDPPPPH1 (APCase ap) = APCase (mkAPPH1 ap)
