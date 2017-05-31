{-# LANGUAGE TemplateHaskell #-}

module NLP.Type.TreeZipper where

import           NLP.Type.PennTreebankII

type Tree = PennTreeGen 

-- | Zipper terminal
data ZTerm c t = PL' | PN' [Tree c t]
  deriving (Show)

-- | Tree Zipper                 
data TreeZipper c t = TZ (ZTerm c t) [(c,[Tree c t],[Tree c t])]
  deriving (Show)

mkRootZipper :: Tree c t -> TreeZipper c t
mkRootZipper (PL x) = TZ PL' []
mkRootZipper (PN x xs) = TZ (PN' xs) []
