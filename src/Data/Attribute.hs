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
, ToTuple (..)
, FromTuple (..)  
) where


import           Data.Discrimination
import           Data.Discrimination.Grouping

  
data AttribList (list :: [*]) where
  AttribNil :: AttribList '[]
  AttribCons :: a -> AttribList as -> AttribList (a ': as)


instance Show (AttribList '[]) where
  show AttribNil = "AttribNil"

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


-- | this is for convenience
class ToTuple a t where
  toTuple :: a -> t 

instance ToTuple (AttribList '[]) () where
  toTuple _ = ()

instance ToTuple (AttribList '[a]) a where
  toTuple (AttribCons x AttribNil) = x

instance ToTuple (AttribList '[a,b]) (a,b) where
  toTuple (AttribCons x (AttribCons y AttribNil)) = (x,y)

instance ToTuple (AttribList '[a,b,c]) (a,b,c) where
  toTuple (AttribCons x (AttribCons y (AttribCons z AttribNil))) = (x,y,z)

instance ToTuple (AttribList '[a,b,c,d]) (a,b,c,d) where
  toTuple (AttribCons x (AttribCons y (AttribCons z (AttribCons w AttribNil))))
    = (x,y,z,w)


class FromTuple t a where  
  fromTuple :: t -> a

instance FromTuple () (AttribList '[]) where
  fromTuple _ = anil

instance FromTuple a (AttribList '[a]) where
  fromTuple x = x `acons` anil

instance FromTuple (a,b) (AttribList '[a,b]) where
  fromTuple (x,y) = x `acons` (y `acons` anil)

instance FromTuple (a,b,c) (AttribList '[a,b,c]) where
  fromTuple (x,y,z) = x `acons` (y `acons` (z `acons` anil))

instance FromTuple (a,b,c,d) (AttribList '[a,b,c,d]) where
  fromTuple (x,y,z,w) = x `acons` (y `acons` (z `acons` (w `acons` anil)))

  

-- (<&>) = AttribCons

acons = AttribCons

infixr 8 `acons`

-- infixr 8 <&>

anil = AttribNil
joinAttrib :: forall b k xs. (Grouping k) =>
              (b -> k)
           -> [b]              
           -> [AttribList (k ': xs)]
           -> [AttribList (k ': Maybe b ': xs)]
joinAttrib f bs lst = joining grouping headMatch f ahead bs lst
  where headMatch :: [b] -> [AttribList (k ': xs)] -> AttribList (k ': Maybe b ': xs)
        headMatch (y:_) (AttribCons k xs : _) = acons k (acons (Just y) xs)
        headMatch _     (AttribCons k xs : _) = acons k (acons Nothing  xs)

