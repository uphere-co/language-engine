{-# LANGUAGE TupleSections #-}

module SRL.Old.Feature.ParseTreePath where

import           Data.List                      (group)
--
import           Data.ListZipper                (ListZipper(..))
import           NLP.Syntax.Util                (phraseType)
import           NLP.Type.PennTreebankII
--
import           SRL.Old.Type
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
      | otherwise = Nothing
    go (x0:[]) (y0:ys)
      | range x0 == range y0 = Just (LZ [] x0 ys)
      | otherwise = Nothing
    go (x0:xs) (y0:[])
      | range x0 == range y0 = Just (LZ xs x0 []) -- (Just x0,xs,[])
      | otherwise = Nothing
    go []     _ys    = Nothing
    go _xs    []     = Nothing


parseTreePathFull :: (Int,Range)
                  -> PennTreeIdxG c (p,a)
                  -> Maybe (ListZipper (PennTreeIdxG c (p,a)))
parseTreePathFull (start,target) tr = elimCommonHead (contain start tr) (containR target tr)


parseTreePathBy :: (PennTreeIdxG c (p,a) -> x)
                -> ListZipper (PennTreeIdxG c (p,a))
                -> [(x,Direction)]
parseTreePathBy f (LZ tostart h totarget) =
  let lst1 = (f h,Down):map ((,Down).f) totarget
      lst2 = map ((,Up).f) . reverse $ tostart
  in lst2 ++ lst1


parseTreePath :: ListZipper (PennTreeIdxG c (p,a)) -> [(Either c p,Direction)]
parseTreePath = parseTreePathBy (snd.phraseType)


simplifyPTP :: (Eq c,Eq p) => [(Either c p, Direction)] -> [(Either c p, Direction)]
simplifyPTP xs = map head (group xs)


simplifyDep :: ListZipper DepInfo -> ListZipper DepInfo
simplifyDep (LZ xs y zs) = LZ (map head (tail (group (y:xs)))) y (map head (tail (group (y:zs))))

