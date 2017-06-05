{-# LANGUAGE MultiWayIf #-}

module SRL.Type where

import           Data.Text               (Text)
--
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           PropBank.Type.Prop
--
import           SRL.CoNLL.CoNLL08.Type


data Position = Before | After | Embed
              deriving (Show,Eq,Ord)

data Direction = Up | Down
               deriving (Show,Eq,Ord)

type ParseTreePath = [(Either ChunkTag POSTag, Direction)]

data Voice = Active | Passive deriving Show

type TreeICP a = Tree (Range,ChunkTag) (Int,(POSTag,a))

type TreeZipperICP a = TreeZipper (Range,ChunkTag) (Int,(POSTag,a))

type ArgNodeFeature = (PropBankLabel,(Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text)))))

type InstanceFeature = (Int,Text,Maybe Voice, [[ArgNodeFeature]])

type Level = Int

(b1,e1) `isPriorTo` (b2,e2) = e1 < b2
r1 `isAfter` r2 = r2 `isPriorTo` r1

r1 `isNotOverlappedWith` r2 = r1 `isPriorTo` r2 || r1 `isAfter` r2

position :: Int -> Range -> Position
position n (b,e) = if | n < b     -> Before
                      | n > e     -> After
                      | otherwise -> Embed

-- | duplicate of comonad is dual to join of monad, i.e. duplicate :: w a -> w (w a)
--   In Tree case, we can make a tree where each node is the subtree at the node point using duplicate 
duplicate :: Tree c a -> Tree (Tree c a) (Tree c a)
duplicate (PN x xs) = PN (PN x xs) (map duplicate xs)
duplicate (PL x) = PL (PL x)


