{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
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
) where


data AttribList (list :: [*]) where
  AttribNull :: AttribList '[]
  AttribCons :: a -> AttribList as -> AttribList (a ': as)


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

-- infixr 8 <&>

anil = AttribNull






  
  

