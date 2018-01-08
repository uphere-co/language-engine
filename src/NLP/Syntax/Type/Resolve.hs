{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}

module NLP.Syntax.Type.Resolve where

import           Control.Lens         ((^.),(^..),_1,_2,to,_Just,_Right)
import           Control.Monad        (guard)
import           Control.Error.Safe   (rightMay)
import           Data.Bifoldable      (bifoldMap)
--
import           Data.Range           (Range)
import           NLP.Syntax.Type.XBar (CompVP(..),Phase(..),X'Tree,CPDPPP(..),SpecCP(..)
                                      ,Coindex(..),TraceType(..),SpecTP(..)
                                      ,SPhase(..)
                                      ,complement,headX,maximalProjection,specifier
                                      ,_SpecCP_Topic,_SpecTP_DP,_SpecTopicP_CP
                                      ,coidx_i,coidx_content
                                      ,hn_range
                                      ,compVPToSpecTP
                                      )

retrieveResolved :: X'Tree 'PH1 -> [(Int,(CompVP 'PH1,Range))]
retrieveResolved x'tr = bifoldMap f f x'tr
  where f x = case x^._2 of
                DPCase dp -> do hn <- dp^..complement._Just.headX
                                i <- hn^..coidx_i._Just
                                [(i,(CompVP_DP (dp^.maximalProjection),hn^.coidx_content.hn_range))]
                CPCase cp -> let lst1 = do spectp <- cp^..complement.specifier
                                           i <- spectp^..coidx_i._Just
                                           spectp1 <- spectp^..coidx_content._Right._SpecTP_DP
                                           [(i,(CompVP_DP spectp1,spectp1))]  -- for the time being, only DP
                                 lst2 = do speccp <- cp^..specifier._Just
                                           guard (case speccp^.coidx_content of SpecCP_Topic _ -> True; _ -> False)
                                           i <- speccp^..coidx_i._Just
                                           rng_cp <- speccp^..coidx_content._SpecCP_Topic._SpecTopicP_CP
                                           [(i,(CompVP_CP rng_cp,rng_cp))]
                             in lst1 ++ lst2
                _         -> []


resolvedCompVP :: [(Int,(CompVP 'PH1,Range))] -> Coindex (Either TraceType (CompVP 'PH1)) -> Maybe (Maybe (TraceType,Int),CompVP 'PH1)
resolvedCompVP resmap c =
  case c of
    Coindex (Just i) (Left t) -> (Just (t,i),) . (^._1) <$> lookup i resmap
    Coindex Nothing  (Left t) -> Nothing
    Coindex _        (Right x) -> Just (Nothing,x)


resolvedSpecTP :: [(Int,(CompVP 'PH1,Range))] -> Coindex (Either TraceType (SpecTP 'PH1)) -> Maybe (Maybe (TraceType,Int),SpecTP 'PH1)
resolvedSpecTP resmap s =
  case s of
    Coindex (Just i) (Left t) -> (Just (t,i),) <$> ((^._1.to (compVPToSpecTP SPH1).to rightMay) =<< lookup i resmap)
    Coindex Nothing  (Left _) -> Nothing
    Coindex _        (Right x) -> Just (Nothing,x)

