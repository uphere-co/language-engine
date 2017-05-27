{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature where

import           Data.Text (Text)
--
import           NLP.Type.PennTreebankII
--
import           SRL.Util


data Position = Before | After | Embed
              deriving (Show,Eq,Ord)

data Direction = Up | Down
               deriving (Show,Eq,Ord)
                          
phraseType :: PennTreeIdxG c p a -> (Range,Either c p)
phraseType (PN (i,c) _) = (i,Left c)
phraseType (PL (i,p) _) = (i,Right p)

position :: Int ->  PennTreeGen c p (Int,a) -> Position
position n tr = let (b,e) = termRange tr
                in if | n < b     -> Before
                      | n > e     -> After
                      | otherwise -> Embed


elimCommonHead :: [PennTreeIdxG c p a]
               -> [PennTreeIdxG c p a]
               -> (Maybe (PennTreeIdxG c p a),[PennTreeIdxG c p a],[PennTreeIdxG c p a])
elimCommonHead lst1 lst2 = go lst1 lst2
  where
    range = fst . phraseType 
    go (x0:x1:xs) (y0:y1:ys) 
      | range x0 == range y0 && range x1 == range y1 = go (x1:xs) (y1:ys)
      | range x0 == range y0 && range x1 /= range y1 = (Just x0,x1:xs,y1:ys)
      | otherwise = (Nothing,x0:x1:xs,y0:y1:ys)
    go (x0:[]) (y0:ys)
      | range x0 == range y0 = (Just x0,[],ys)
      | otherwise = (Nothing,[x0],y0:ys)
    go (x0:xs) (y0:[])
      | range x0 == range y0 = (Just x0,xs,[])
      | otherwise = (Nothing,x0:xs,[y0])
    go []     ys     = (Nothing,[],ys)
    go xs     []     = (Nothing,xs,[])

parseTreePathFull :: (Int,Range) -> PennTreeIdxG c p a
              -> (Maybe (PennTreeIdxG c p a),[PennTreeIdxG c p a],[PennTreeIdxG c p a])
parseTreePathFull (start,target) tr = elimCommonHead (contain start tr) (containR target tr)

parseTreePath :: (Maybe (PennTreeIdxG c p a),[PennTreeIdxG c p a],[PennTreeIdxG c p a])
                        -> [(Either c p,Direction)]
parseTreePath (mh,tostart,totarget) =
  case mh of
    Nothing -> []
    Just h -> let lst1 = ((snd.phraseType) h,Down):map ((,Down).snd.phraseType) totarget
                  lst2 = map ((,Up).snd.phraseType) . reverse $ tostart
              in lst2 ++ lst1
