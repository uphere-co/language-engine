{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature.Dependency where

import           Control.Lens            hiding (levels,Level)
import           Data.Discrimination
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (minimumBy,sortBy)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import           Data.Tree                      (levels)
--
import           CoreNLP.Simple.Type.Simplified
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
import qualified NLP.Type.UniversalDependencies2.Syntax as UD (DependencyRelation(ROOT))
import           PropBank.Util
--
import           SRL.Feature.ParseTreePath
import           SRL.Type
import           SRL.Util
--


annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Level,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))


levelMap :: Dependency -> IntMap Int
levelMap (Dependency root nods edgs0) = 
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs0
      searchtree = head (dfs (buildG bnds edgs) [root])
  in IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]


motherMap :: Dependency -> IntMap (Int,DependencyRelation)
motherMap (Dependency root _nods edgs0) =
  let edgs = ((0,root),UD.ROOT) : edgs0
  in IM.fromList (map (\((mother,daughter),rel) -> (daughter-1,(mother-1,rel))) edgs)


decorateLeaves :: IntMap v -> Bitree c (Int,t) -> Bitree c (Int,(Maybe v,t))
decorateLeaves m tr = let lkup (n,t) = (n,(IM.lookup n m,t)) in fmap lkup tr
 

depTree :: Dependency -> Bitree c (Int,t) -> Bitree c (Int,(Maybe (Int,DependencyRelation),t))
depTree dep tr = decorateLeaves (motherMap dep) tr


depLevelTree :: Dependency -> Bitree c (Int,t) -> Bitree c (Int,(Maybe Level,t))
depLevelTree dep tr = decorateLeaves (levelMap dep) tr


joiningIntMap :: IntMap v -> IntMap v' -> IntMap (v,v')
joiningIntMap m1 m2 =
  IM.fromList (joining grouping (\as bs -> (fst (head as), (snd (head as),snd (head bs)))) fst fst (IM.toAscList m1) (IM.toAscList m2))


rightOuterIntMap :: IntMap v -> IntMap v' -> IntMap (Maybe v,v')
rightOuterIntMap m1 m2 =
  let l1 = IM.toAscList m1
      l2 = IM.toAscList m2
      rss = rightOuter grouping (\a b->(a^._1, (Just (a^._2), b^._2))) (\b->(b^._1,(Nothing,b^._2))) fst fst l1 l2
  in IM.fromList (concat rss)


headWord :: PennTreeIdxG ChunkTag (Maybe Level,(POSTag,Text)) -> Maybe (Int,(Level,(POSTag,Text)))
headWord  = safeHead . sortBy (compare `on` view (_2._1))
          . mapMaybe (\(i,(ml,postxt)) -> fmap (i,) (fmap (,postxt) ml)) . getLeaves 


cmpLevel :: (Ord a) => Maybe a -> Maybe a -> Ordering
cmpLevel Nothing  _        = GT
cmpLevel _        Nothing  = LT
cmpLevel (Just x) (Just y) = compare x y


annotateDepInfo :: PennTreeIdxG ChunkTag (Maybe DepInfo,(POSTag,Text))
                     -> PennTreeIdxG (ChunkTag,Maybe DepInfo) (Maybe DepInfo,(POSTag,Text))
annotateDepInfo (PL t)        = PL t
annotateDepInfo (PN (r,x) xs) =
  let ys = map annotateDepInfo xs
      zs :: [Either (ChunkTag,Maybe DepInfo) (Maybe DepInfo)]
      zs = map getTag ys
      y = (x,minimumBy (cmpLevel `on` (fmap (view dinfo_level))) (map (either snd id) zs))
  in PN (r,y) ys


depInfoTree :: Dependency
            -> PennTreeIdxG ChunkTag (POSTag,Text)
            -> PennTreeIdxG (ChunkTag,Maybe DepInfo) (Maybe DepInfo,(POSTag,Text))
depInfoTree dep tr = let tr' = decorateLeaves (rightOuterIntMap (levelMap dep) (motherMap dep)) tr
                         conv :: (Int,(Maybe (Maybe Level, (Int,DependencyRelation)),(POSTag,Text))) -> (Int,(Maybe DepInfo,(POSTag,Text)))
                         conv (i,(Just (ml,(m,rel)),pt)) = (i,(Just (DepInfo i m rel ml),pt))
                         conv (i,(Nothing,pt))           = (i,(Nothing,pt))
                         tr'' = fmap conv tr'
                     in annotateDepInfo tr''


depRelPath :: Dependency -> PennTreeIdxG ChunkTag (POSTag,Text) -> (Int,Range) -> Maybe (ListZipper DepInfo)
depRelPath dep itr (start,target) =
  let ditr = depInfoTree dep itr
      mdptp = parseTreePathFull (start,target) ditr
      f (PL (_n,(md,_)))     = md 
      f (PN (_rng,(_,md)) _) = md 
  in case mdptp of
       Nothing -> Nothing
       Just dptp -> let LZ xp x xn = fmap f dptp
                    in Just (simplifyDep (LZ (catMaybes xp) (fromJust x) (catMaybes xn)))
