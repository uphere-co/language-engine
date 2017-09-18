{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.BitreeZipper where

import           Control.Lens
import           Data.Bifoldable         (biList)
import           Data.List               (find,unfoldr)
--
import           Data.Bitree
--

-- | Surrounding context of the focused item at current level
data BitreeContext c t = TC { _tc_node_content :: c
                          , _tc_prevs :: [Bitree c t]
                          , _tc_nexts :: [Bitree c t]
                          }
                     deriving (Show)

makeLenses ''BitreeContext


-- | Zipper for tree
data BitreeZipper c t = TZ { _tz_current  :: Bitree c t          -- ^ current item
                         , _tz_contexts :: [BitreeContext c t] -- ^ recusively defined
                                                             --   contexts of current item
                         }
                    deriving (Show)

makeLenses ''BitreeZipper


data ListZipper a = LZ { _lz_prevs :: [a]
                       , _lz_current :: a
                       , _lz_nexts :: [a]
                       }
                  deriving (Show,Functor)

makeLenses ''ListZipper


genListZippers :: [a] -> [ListZipper a]
genListZippers [] = error "cannot make a zipper for empty list"
genListZippers (k:ks) = l1 : unfoldr succ l1
  where l1 = LZ [] k ks
        succ (LZ xs y [])     = Nothing
        succ (LZ xs y (z:zs)) = let w = LZ (y:xs) z zs in Just (w,w)


mkBitreeZipper :: [BitreeContext c t] -> Bitree c t -> Bitree (BitreeZipper c t) (BitreeZipper c t)
mkBitreeZipper zs p@(PL x)    = PL (TZ p zs)
mkBitreeZipper zs p@(PN x xs) = PN (TZ p zs) lst
  where lst = map (\(LZ xs1 y xs2) -> mkBitreeZipper ((TC x xs1 xs2):zs) y) (genListZippers xs)


extractZipperById :: (Eq i) => i -> Bitree (i,a) (i,a) -> Maybe (BitreeZipper (i,a) (i,a))
extractZipperById rng tr = find (\z -> fst (getRoot1 (current z)) == rng) $ biList (mkBitreeZipper [] tr)


current :: BitreeZipper c t -> Bitree c t
current (TZ x _) = x

prev :: BitreeZipper c t -> Maybe (BitreeZipper c t)
prev (TZ x (y:ys)) = case y of
                       TC c (z:zs) ws -> Just (TZ z ((TC c zs (x:ws)):ys))
                       _              -> Nothing
prev _             = Nothing

next :: BitreeZipper c t -> Maybe (BitreeZipper c t)
next (TZ x (y:ys)) = case y of
                       TC c zs (w:ws) -> Just (TZ w ((TC c (x:zs) ws):ys))
                       _              -> Nothing
next _             = Nothing

parent :: BitreeZipper c t -> Maybe (BitreeZipper c t)
parent (TZ x (y:ys)) =
  case y of
    TC c zs ws -> Just (TZ (PN c (f (x:ws))) ys)
      where f = foldr (\x acc -> acc . (x:)) id zs
parent _             = Nothing


child1 :: BitreeZipper c t -> Maybe (BitreeZipper c t)
child1 (TZ (PL _) _) = Nothing
child1 (TZ (PN c (x:xs)) ys) = Just (TZ x ((TC c [] xs):ys))



root :: BitreeZipper c t -> BitreeZipper c t
root z = last (z : unfoldr (\x -> parent x >>= \y -> Just (y,y)) z)


-- | unfocus to original tree structure
--
toBitree :: BitreeZipper c t -> Bitree c t
toBitree = current . root


-- | replace the whole subtree focused by zipper
--
replaceFocusTree :: (Bitree c t -> Bitree c t) -> BitreeZipper c t -> BitreeZipper c t
replaceFocusTree f = tz_current %~ f


-- | replace the root item of the subtree focused by zipper. 
--
replaceFocusItem :: (c -> c) -> (t -> t) -> BitreeZipper c t -> BitreeZipper c t
replaceFocusItem f g z = let tr = case z^.tz_current of
                                 PN c xs -> PN (f c) xs
                                 PL t    -> PL (g t)
                    in replaceFocusTree (const tr) z


-- | Remove currently focused item and return tree.
--   If it is a single leaf tree, then Nothing.
--
removeFocusTree :: BitreeZipper c t -> Maybe (Bitree c t)
removeFocusTree (TZ _ [])                = Nothing
removeFocusTree (TZ _ ((TC c ps ns):xs)) = let x = PN c (reverse ps ++ ns)
                                           in Just (toBitree (TZ x xs))

