{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}

module NLP.Syntax.Type.Resolve where

import           Control.Lens         ((^?),(^.),(^..),_1,_2,to,_Just,_Right,makePrisms)
import           Control.Monad        (guard,mzero)
import           Control.Error.Safe   (rightMay)
import           Data.Bifoldable      (bifoldMap)
import           Data.Function        (on)
import           Data.List            (groupBy,sortBy)
--
import           Data.Range           (Range)
import           NLP.Syntax.Type.XBar (CompVP(..),Phase(..),X'Tree,CPDPPP(..),SpecCP(..)
                                      ,Coindex(..),TraceType(..),SpecTP(..),SpecTopicP(..)
                                      ,SPhase(..)
                                      ,complement,headX,maximalProjection,specifier
                                      ,_SpecCP_Topic,_SpecTP_DP,_SpecTopicP_CP
                                      ,coidx_i,coidx_content
                                      ,hn_range
                                      ,compVPToSpecTP
                                      )


data Resolved c = RBound c Range  -- ^  resolved maximal projection and head range
                | RFree_WHDP Range
                deriving (Show,Eq,Ord,Functor)

makePrisms ''Resolved

resolved2Range (RBound _ rng)   = rng
resolved2Range (RFree_WHDP rng) = rng

data Referent c = RefRExp c
                | RefVariable (TraceType,Int) (Resolved c)
                deriving (Show,Functor)

makePrisms ''Referent


referent2CompVP :: Referent (CompVP 'PH1) -> CompVP 'PH1
referent2CompVP (RefRExp x)                    = x
referent2CompVP (RefVariable _ (RBound x _))   = x
referent2CompVP (RefVariable _ (RFree_WHDP r)) = CompVP_DP r


referent2Trace :: Referent a -> Maybe (TraceType,Int)
referent2Trace (RefVariable t _) = Just t
referent2Trace _                 = Nothing


retrieveResolved :: X'Tree 'PH1 -> [(Int,Resolved (CompVP 'PH1))]
retrieveResolved = map (head . sortBy (compare `on` (^._2))) . groupBy ((==) `on` (^._1)) .  bifoldMap f f
  where f x = case x^._2 of
                DPCase dp -> do hn <- dp^..complement._Just.headX
                                i <- hn^..coidx_i._Just
                                let resolved = RBound (CompVP_DP (dp^.maximalProjection)) (hn^.coidx_content.hn_range)
                                [(i,resolved)]
                CPCase cp -> let lst1 = do spectp <- cp^..complement.specifier
                                           i <- spectp^..coidx_i._Just
                                           spectp1 <- spectp^..coidx_content._Right._SpecTP_DP
                                           let resolved = RBound (CompVP_DP spectp1) spectp1
                                           [(i,resolved)]  -- for the time being, only DP
                                 lst2 = do speccp <- cp^..specifier._Just
                                           i <- speccp^..coidx_i._Just
                                           -- guard (case speccp^.coidx_content of SpecCP_Topic _ -> True; _ -> False)
                                           case speccp^.coidx_content of
                                             SpecCP_Topic (SpecTopicP_CP rng_cp) -> do
                                               -- rng_cp <- speccp^..coidx_content._SpecCP_Topic._SpecTopicP_CP
                                               let resolved = RBound (CompVP_CP rng_cp) rng_cp
                                               [(i,resolved)]
                                             SpecCP_WH rng_wh -> do
                                               let resolved = RFree_WHDP rng_wh  -- assume WH-form as DP for the time being
                                               [(i,resolved)]
                                             _ -> []
                             in lst1 ++ lst2
                _         -> []



resolvedCompVP :: [(Int,Resolved (CompVP 'PH1))]
               -> Coindex (Either TraceType (CompVP 'PH1))
               -> Maybe (Referent (CompVP 'PH1)) --  (Maybe (TraceType,Int),Either (CompVP 'PH1) (Resolved (CompVP 'PH1)))
resolvedCompVP resmap c =
  case c of
    Coindex (Just i) (Left t) -> RefVariable (t,i) <$> lookup i resmap
    Coindex Nothing  (Left t) -> mzero
    Coindex _        (Right x) -> return (RefRExp x)


resolvedSpecTP :: [(Int,Resolved (CompVP 'PH1))]
               -> Coindex (Either TraceType (SpecTP 'PH1))
               -> Maybe (Referent (SpecTP 'PH1))
resolvedSpecTP resmap s =
  case s of
    Coindex (Just i) (Left t) -> do
      r <- lookup i resmap
      case r of
        RBound c rng     -> RefVariable (t,i) . flip RBound rng <$> rightMay (compVPToSpecTP SPH1 c)
        RFree_WHDP rng -> return (RefVariable (t,i) (RFree_WHDP rng))
    Coindex Nothing  (Left _)  -> mzero
    Coindex _        (Right x) -> return (RefRExp x)


