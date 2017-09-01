{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module SRL.Old.Feature.Dependency where

import           Control.Lens            hiding (levels,Level)
import           Data.Function                  (on)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (minimumBy,sortBy)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import           Data.Tree                      (levels)
--
import           Data.Attribute
import           Data.Bitree                    (getLeaves)
import           Data.BitreeZipper
import           NLP.Type.CoreNLP
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
import qualified NLP.Type.UniversalDependencies2.Syntax as UD (DependencyRelation(ROOT))
--
import           SRL.Old.Feature.ParseTreePath
import           SRL.Old.Type
import           SRL.Old.Util
--


annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Level,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))


levelMap :: Dependency -> IntMap Int
levelMap dep = 
  let searchtree = dependencyIndexTree dep
  in IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]


motherMap :: Dependency -> IntMap (Int,DependencyRelation)
motherMap (Dependency root _nods edgs0) =
  let edgs = ((0,root),UD.ROOT) : edgs0
  in IM.fromList (map (\((mother,daughter),rel) -> (daughter-1,(mother-1,rel))) edgs)



depTree :: Dependency
        -> PennTreeIdxG n (ALAtt ls)
        -> PennTreeIdxG n (ALAtt (Maybe (Int,DependencyRelation) ': ls))
depTree dep tr = decorateLeaves (motherMap dep) tr


depLevelTree :: Dependency
             -> PennTreeIdxG n (ALAtt ls)
             -> PennTreeIdxG n (ALAtt (Maybe Level ': ls))
depLevelTree dep tr = decorateLeaves (levelMap dep) tr




headWord :: PennTreeIdxG ChunkTag (Maybe Level,(POSTag,Text)) -> Maybe (Int,(Level,(POSTag,Text)))
headWord  = safeHead . sortBy (compare `on` view (_2._1))
          . mapMaybe (\(i,(ml,postxt)) -> fmap (i,) (fmap (,postxt) ml)) . getLeaves 


cmpLevel :: (Ord a) => Maybe a -> Maybe a -> Ordering
cmpLevel Nothing  _        = GT
cmpLevel _        Nothing  = LT
cmpLevel (Just x) (Just y) = compare x y


annotateDepInfo :: PennTreeIdxG (ANAtt ns) (ALAtt (Maybe DepInfo ': ls))
                -> PennTreeIdxG (ANAtt (Maybe DepInfo ': ns)) (ALAtt (Maybe DepInfo ': ls))
annotateDepInfo (PL t)        = PL t
annotateDepInfo (PN (r,ANode c x) xs) =
  let ys = map annotateDepInfo xs
      zs = map (\case PN (_,z) _ -> ahead (getAnnot z); PL (_,z) -> ahead (getAnnot z)) ys
      y = ANode c (minimumBy (cmpLevel `on` (fmap (view dinfo_level))) zs `acons` x)
  in PN (r,y) ys


depInfoTree :: forall ns ls .
               Dependency
            -> PennTreeIdxG (ANAtt ns) (ALAtt ls)
            -> PennTreeIdxG (ANAtt (Maybe DepInfo ': ns)) (ALAtt (Maybe DepInfo ': ls))
depInfoTree dep tr = let tr' = decorateLeaves (rightOuterIntMap (levelMap dep) (motherMap dep)) tr
                         conv :: (Int,ALAtt ((Maybe (Maybe Level, (Int,DependencyRelation))) ': ls))
                              -> (Int,ALAtt (Maybe DepInfo ': ls))
                         conv (i,ALeaf pt x) = (i,ALeaf pt (f x))
                           where
                                 f (AttribCons (Just (ml,(m,rel))) xs) = AttribCons (Just (DepInfo i m rel ml)) xs
                                 f (AttribCons Nothing             xs) = AttribCons Nothing                     xs 
                         tr'' = fmap conv tr'
                     in annotateDepInfo tr''


depRelPath :: Dependency
           -> PennTreeIdxG ChunkTag (POSTag,Text)
           -> (Int,Range)
           -> Maybe (ListZipper DepInfo)
depRelPath dep itr (start,target) =
  let ditr = depInfoTree dep (mkAnnotatable itr)
      mdptp = parseTreePathFull (start,target) (bimap convn convl ditr)
      convn (rng,ANode c  x) = (rng,(c,ahead x))
      convl (i  ,ALeaf pt x) = (i  ,(ahead x,pt))
      f (PL (_n  ,(md,_)))   = md 
      f (PN (_rng,(_,md)) _) = md 
  in case mdptp of
       Nothing -> Nothing
       Just dptp -> let LZ xp x xn = fmap f dptp
                    in Just (simplifyDep (LZ (catMaybes xp) (fromJust x) (catMaybes xn)))
