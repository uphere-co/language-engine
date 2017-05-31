{-# LANGUAGE TemplateHaskell #-}

module NLP.Type.TreeZipper where

import           Data.List               (unfoldr)
--
import           NLP.Type.PennTreebankII

type Tree = PennTreeGen 

-- | Zipper terminal
data ZTerm c t = PL' | PN' [Tree c t]
  deriving (Show)

-- | Tree Zipper                 
data TreeZipper c t = TZ (ZTerm c t) [(c,[Tree c t],[Tree c t])]
  deriving (Show)

{- 
mkRootZipper :: Tree c t -> TreeZipper c t
mkRootZipper (PL x) = TZ PL' []
mkRootZipper (PN x xs) = TZ (PN' xs) []
-}
 
mkZipper :: [(c,[Tree c t],[Tree c t])] -> Tree c t -> Tree (TreeZipper c t) (TreeZipper c t)
mkZipper zs (PL x) = PL (TZ PL' zs)
mkZipper zs (PN x xs) = PN (TZ (PN' xs) zs) (map (\(xs1,y,xs2) -> mkZipper ((x,xs1,xs2):zs) y) (mkListZipper xs))


mkListZipper [] = error "cannot make a zipper for empty list"
mkListZipper (k:ks) = unfoldr next ([],k,ks)
  where next (xs,y,z:zs) = let w = (y:xs,z,zs) in Just (w,w)
        next (xs,y,[]) = Nothing
 

{- 
mkListZipper xs =
  where go ys [] = ys
        go ys (x:xs) = 

  error "cannot treat null"
mkListZipper (x:xs) = x
-}
