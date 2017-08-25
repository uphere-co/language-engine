{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Lexicon.Merge where

import           Control.Lens                  ((^.),(%~),(.~),_1,_2)
import           Data.Function                 (on)
import qualified Data.HashSet            as HS
import           Data.List                     (groupBy,sortBy,tails)
import           Data.Maybe                    (isNothing,mapMaybe,maybeToList)
import           Data.Text                     (Text)
--
import           NLP.Type.PennTreebankII       (TernaryLogic(..))
import           NLP.Type.SyntaxProperty       (Voice(..))
--
import           Lexicon.Type                  (ArgPattern(..), GArg(..), GRel(..)
                                               ,patt_property
                                               ,patt_arg0
                                               ,patt_arg1
                                               ,patt_arg2
                                               ,patt_arg3
                                               ,patt_arg4
                                               )


convertPassive :: ArgPattern Voice GRel -> ArgPattern () GRel
convertPassive patt =
    case patt^.patt_property of
      Just Passive -> ArgPattern
                        (Just ())
                        (fmap conv (patt^.patt_arg0))
                        (fmap conv (patt^.patt_arg1))
                        (fmap conv (patt^.patt_arg2))
                        (fmap conv (patt^.patt_arg3))
                        (fmap conv (patt^.patt_arg4))
      _            -> ArgPattern
                        (Just ())
                        (patt^.patt_arg0)
                        (patt^.patt_arg1)
                        (patt^.patt_arg2)
                        (patt^.patt_arg3)
                        (patt^.patt_arg4)

  where
    conv (GR_PP (Just "by"))  = GR_NP (Just GASBJ)
    conv (GR_NP (Just GASBJ)) = GR_NP (Just GA1)
    conv x                    = x



convertNP :: ArgPattern () GRel -> ArgPattern () GRel
convertNP patt =
  let arglst = [patt^.patt_arg0,patt^.patt_arg1,patt^.patt_arg2,patt^.patt_arg3,patt^.patt_arg4]
  in if | not (Just (GR_NP (Just GASBJ)) `elem` arglst) && patt^.patt_arg0 == Just (GR_NP Nothing) -> (patt_arg0 .~ Just (GR_NP (Just GASBJ))) patt
        | not (Just (GR_NP (Just GA1)) `elem` arglst) && patt^.patt_arg1 == Just (GR_NP Nothing) -> (patt_arg1 .~ Just (GR_NP (Just GA1))) patt
        | otherwise                                                                                -> patt


mergeS (GR_S x) = GR_SBAR x
mergeS x        = x


convertS :: ArgPattern () GRel -> ArgPattern () GRel
convertS = (patt_arg0 %~ fmap mergeS)
         . (patt_arg1 %~ fmap mergeS)
         . (patt_arg2 %~ fmap mergeS)
         . (patt_arg3 %~ fmap mergeS)
         . (patt_arg4 %~ fmap mergeS)

convertSBAR :: ArgPattern () GRel -> ArgPattern () GRel
convertSBAR patt =
  let arglst = [patt^.patt_arg0,patt^.patt_arg1,patt^.patt_arg2,patt^.patt_arg3,patt^.patt_arg4]
  in if | not (Just (GR_S (Just GASBJ)) `elem` arglst)  && patt^.patt_arg0 == Just (GR_SBAR Nothing) -> (patt_arg0 .~ Just (GR_SBAR (Just GASBJ))) patt
        | not (Just (GR_SBAR (Just GA1)) `elem` arglst) && patt^.patt_arg1 == Just (GR_SBAR Nothing) -> (patt_arg1 .~ Just (GR_SBAR (Just GA1))) patt
        | otherwise                                                                                  -> patt


mergePatterns :: [(ArgPattern Voice GRel,Int)] -> [(ArgPattern () GRel,Int)]
mergePatterns pattstats0 =
  let pgrps = groupBy ((==) `on` (^._1))
            . sortBy (compare `on` (^._1))
            . map (_1 %~ convertSBAR . convertNP . convertS . convertPassive)
            $ pattstats0
  in sortBy (flip compare `on` (^._2)) . map (\xs -> (fst (head xs), sum (map snd xs))) $ pgrps


pattern SubPatternOf   = Yes
pattern SuperPatternOf = No
pattern Neither        = Unclear


patternRelation :: ArgPattern () GRel -> ArgPattern () GRel -> TernaryLogic
patternRelation x y | x `isSubPatternOf` y = SubPatternOf
                    | y `isSubPatternOf` x = SuperPatternOf
                    | otherwise            = Neither
  where
    each l x y  = (isNothing (x^.l)) || (x^.l == y^.l)
    isSubPatternOf x y = each patt_arg0 x y &&
                         each patt_arg1 x y &&
                         each patt_arg2 x y &&
                         each patt_arg3 x y &&
                         each patt_arg4 x y

patternGraph :: (a -> a -> TernaryLogic) -> [(Int,a)] -> [(Int,Int)]
patternGraph f lst = (concatMap (\(x:xs) -> mapMaybe (link x) (x:xs)) . init . tails) lst
  where
    link (i,x) (j,y) = case f x y of
                         SubPatternOf   -> Just (i,j)
                         SuperPatternOf -> Just (j,i)
                         Neither        -> Nothing


listOfSupersetSubset :: [(Int,Int)] -> [(Int,[Int],[Int])]
listOfSupersetSubset xs = let is = map fst xs
                          in map (\i -> (i,superset i xs,subset i xs)) is
  where superset i = map snd . filter (\x -> fst x == i)
        subset   i = map fst . filter (\x -> snd x == i)



topPatterns :: [(Int,(ArgPattern () GRel,Int))]
            -> [(Int,[Int],[Int])]
            -> [(ArgPattern () GRel,Int)]
topPatterns ipatts slst = do
    (topi,_,subis) <- filter (\x -> length (x^._2) == 1) slst
    top <- maybeToList (lookup topi ipatts)
    let subs = mapMaybe (\s -> lookup s ipatts) subis
        n = sum (map snd subs)
    return (top^._1,n)
