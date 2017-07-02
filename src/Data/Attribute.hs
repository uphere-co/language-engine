{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Attribute
( anil
, acons
, ahead
, atail
, Elem(..)
, getElem
, AttribList(..)
, joinAttrib
) where


import           Data.Discrimination
import           Data.Discrimination.Grouping
import           Data.Maybe

  
data AttribList (list :: [*]) where
  AttribNull :: AttribList '[]
  AttribCons :: a -> AttribList as -> AttribList (a ': as)


instance Show (AttribList '[]) where
  show AttribNull = "AttribNull"

instance (Show a, Show (AttribList as)) => Show (AttribList (a ': as)) where
  show (AttribCons x xs) = "AttribCons (" ++ show x ++ ") (" ++ show xs ++ ")" 

-- | following https://stackoverflow.com/questions/37283403/type-level-environment-in-haskell
--   but the original post had a fault. 
data Elem x xs where
  Here :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

getElem :: Elem x xs -> AttribList xs -> x
getElem Here      (AttribCons x _)  = x
getElem (There e) (AttribCons _ xs) = getElem e xs 

{- 
data (:~:) a b where
  Refl ::  a :~: a

type family Position x xs :: Elem x xs where
-}

instance Show (Elem x xs) where
  show Here = "Here"
  show (There x) = "There (" ++ show x ++ ")"


ahead :: AttribList (a ': as) -> a
ahead (AttribCons x _) = x


atail :: AttribList (a ': as) -> AttribList as
atail (AttribCons _ xs) = xs


-- (<&>) = AttribCons

acons = AttribCons

infixr 8 `acons`

-- infixr 8 <&>

anil = AttribNull


joinAttrib :: forall b k xs. (Grouping k) =>
              (b -> k)
           -> [b]              
           -> [AttribList (k ': xs)]
           -> [AttribList (k ': Maybe b ': xs)]
joinAttrib f bs lst = catMaybes (joining grouping headMatch f ahead bs lst)
  where headMatch :: [b] -> [AttribList (k ': xs)] -> Maybe (AttribList (k ': Maybe b ': xs))
        headMatch (y:_) (AttribCons k xs : _) = Just (acons k (acons (Just y) xs))
        headMatch _     (AttribCons k xs : _) = Just (acons k (acons Nothing  xs))
        headMatch _     _                     = Nothing
