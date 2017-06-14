{-# LANGUAGE TupleSections #-}

module SRL.Feature.ParseTreePath where

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
import           PropBank.Type.Prop
--
import           SRL.Feature.Util
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--

elimCommonHead :: [PennTreeIdxG c (p,a)]
               -> [PennTreeIdxG c (p,a)]
               -> Maybe (ListZipper (PennTreeIdxG c (p,a)))
elimCommonHead lst1 lst2 = go lst1 lst2
  where
    range = fst . phraseType
    go :: [PennTreeIdxG c (p,a)] -> [PennTreeIdxG c (p,a)] -> Maybe (ListZipper (PennTreeIdxG c (p,a)))
    go (x0:x1:xs) (y0:y1:ys) 
      | range x0 == range y0 && range x1 == range y1 = go (x1:xs) (y1:ys)
      | range x0 == range y0 && range x1 /= range y1 = Just (LZ (x1:xs) x0 (y1:ys))
      | otherwise = Nothing -- error "elimCommonHead" -- (Nothing,x0:x1:xs,y0:y1:ys)  -- this should not happen in a single tree.
    go (x0:[]) (y0:ys)
      | range x0 == range y0 = Just (LZ [] x0 ys)
      | otherwise = Nothing -- error "elimCommonHead" -- (Nothing,[x0],y0:ys)  -- this should not happen in a single tree
    go (x0:xs) (y0:[])
      | range x0 == range y0 = Just (LZ xs x0 []) -- (Just x0,xs,[])
      | otherwise = Nothing -- error "elimCommonHead" -- (Nothing,x0:xs,[y0]) -- this should not happen in a single tree
    go []     ys     = Nothing -- error "elimCommonHead" -- (Nothing,[],ys)
    go xs     []     = Nothing -- error "elimCommonHead" -- (Nothing,xs,[])


parseTreePathFull :: (Int,Range)
                  -> PennTreeIdxG c (p,a)
                  -> Maybe (ListZipper (PennTreeIdxG c (p,a)))
                     -- (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
parseTreePathFull (start,target) tr = elimCommonHead (contain start tr) (containR target tr)


parseTreePathBy :: (PennTreeIdxG c (p,a) -> x)
                -> ListZipper (PennTreeIdxG c (p,a))
                                      -- (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
                -> [(x,Direction)]
parseTreePathBy f (LZ tostart h totarget) =
  let lst1 = (f h,Down):map ((,Down).f) totarget
      lst2 = map ((,Up).f) . reverse $ tostart
  in lst2 ++ lst1

{-      
  case mh of
    Nothing -> []
    Just h -> 
-}

parseTreePath :: ListZipper (PennTreeIdxG c (p,a))  -- (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
              -> [(Either c p,Direction)]
parseTreePath = parseTreePathBy (snd.phraseType)


simplifyPTP :: (Eq c,Eq p) => [(Either c p, Direction)] -> [(Either c p, Direction)]
simplifyPTP xs = map head (group xs)


simplifyDep :: ListZipper DepInfo -> ListZipper DepInfo
simplifyDep (LZ xs y zs) = LZ (map head (tail (group (y:xs)))) y (map head (tail (group (y:zs))))

