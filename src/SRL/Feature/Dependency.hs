{-# LANGUAGE TupleSections #-}

module SRL.Feature.Dependency where

import           Control.Lens            hiding (levels,Level)
import           Control.Monad                  ((<=<),guard)
import           Data.Bifunctor                 (bimap)
import           Data.Bifoldable                (biList)
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy)
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
import           PropBank.Type.Prop
--
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--


annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Level,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))

levelMap :: Dependency -> Tree c (Int,t) -> IntMap Int
levelMap (Dependency root nods edgs') tr = 
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
      searchtree = head (dfs (buildG bnds edgs) [root])
  in IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]

motherMap :: Dependency -> Tree c (Int,t) -> IntMap (Int,DependencyRelation)
motherMap (Dependency root nods edgs') tr = IM.fromList (map (\((mother,daughter),rel) -> (daughter-1,(mother-1,rel))) edgs')


decorateLeaves :: IntMap v -> Tree c (Int,t) -> Tree c (Int,(Maybe v,t))
decorateLeaves m tr = let lkup (n,t) = (n,(IM.lookup n m,t)) in fmap lkup tr

 
depTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe (Int,DependencyRelation),t))
depTree dep tr = decorateLeaves (motherMap dep tr) tr

  {- let dmap = motherMap dep tr 
                     lkup (n,txt) = (n,(IM.lookup n dmap,txt))
                 in fmap lkup tr -}


depLevelTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe Level,t))
depLevelTree dep tr = decorateLeaves (levelMap dep tr) tr

  -- fmap (annotateLevel (levelMap dep tr)) tr
{- 
        bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
      searchtree = head (dfs (buildG bnds edgs) [root])
      levelMap = IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]
  in fmap (annotateLevel levelMap) tr 
-}

headWord :: PennTreeIdxG ChunkTag (Maybe Level,(POSTag,Text)) -> Maybe (Int,(Level,(POSTag,Text)))
headWord  = safeHead . sortBy (compare `on` view (_2._1))
          . mapMaybe (\(i,(ml,postxt)) -> fmap (i,) (fmap (,postxt) ml)) . getLeaves 
     
  
  -- let bnds = let xs = map fst nods in (minimum xs, maximum xs)
  --     edgs = map fst edgs'
  --     searchtree = head (dfs (buildG bnds edgs) [root])
  --     levelMap = IM.fromList  $ map (\(i,v) -> (i-1,v)) $ concat $ zipWith3 (\xs n dep -> map (,(n,dep)) xs) (levels searchtree) [0..]
  -- in fmap (annotateLevel levelMap) tr 


