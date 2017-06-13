{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature.Dependency where

import           Control.Arrow                  (second)
import           Control.Lens            hiding (levels,Level)
import           Control.Monad                  ((<=<),guard)
import           Data.Bifunctor                 (bimap)
import           Data.Bifoldable                (biList)
import           Data.Discrimination
import           Data.Either                    (lefts)
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,minimumBy,sortBy)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import           Data.Tree                      (levels)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple.Convert                      (cutf8)
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           NLP.Type.UniversalDependencies2.Syntax
import qualified NLP.Type.UniversalDependencies2.Syntax as UD (DependencyRelation(ROOT))
import           PropBank.Type.Prop
import           PropBank.Util
--
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--
import Debug.Trace


data DepInfo = DepInfo { _dinfo_self :: Int
                       , _dinfo_mother :: Int
                       , _dinfo_rel :: DependencyRelation
                       , _dinfo_level :: Maybe Int }
               deriving Show


makeLenses ''DepInfo

annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Level,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))


levelMap :: Dependency -> Tree c (Int,t) -> IntMap Int
levelMap (Dependency root nods edgs0) tr = 
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs0
      searchtree = head (dfs (buildG bnds edgs) [root])
  in IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]


motherMap :: Dependency -> Tree c (Int,t) -> IntMap (Int,DependencyRelation)
motherMap (Dependency root nods edgs0) tr =
  let edgs = ((0,root),UD.ROOT) : edgs0
  in IM.fromList (map (\((mother,daughter),rel) -> (daughter-1,(mother-1,rel))) edgs)


decorateLeaves :: IntMap v -> Tree c (Int,t) -> Tree c (Int,(Maybe v,t))
decorateLeaves m tr = let lkup (n,t) = (n,(IM.lookup n m,t)) in fmap lkup tr
 

depTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe (Int,DependencyRelation),t))
depTree dep tr = decorateLeaves (motherMap dep tr) tr


depLevelTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe Level,t))
depLevelTree dep tr = decorateLeaves (levelMap dep tr) tr


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
  in trace (show y) $ PN (r,y) ys


depInfoTree :: Dependency -> PennTreeIdxG ChunkTag (POSTag,Text)
            -> PennTreeIdxG (ChunkTag,Maybe DepInfo) (Maybe DepInfo,(POSTag,Text))
depInfoTree dep tr = let tr' = decorateLeaves (rightOuterIntMap (levelMap dep tr) (motherMap dep tr)) tr
                         conv :: (Int,(Maybe (Maybe Level, (Int,DependencyRelation)),(POSTag,Text))) -> (Int,(Maybe DepInfo,(POSTag,Text)))
                         conv (i,(Just (ml,(m,rel)),pt)) = (i,(Just (DepInfo i m rel ml),pt))
                         conv (i,(Nothing,pt))           = (i,(Nothing,pt))
                         tr'' = fmap conv tr'
                     in annotateDepInfo tr''
