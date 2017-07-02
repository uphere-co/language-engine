{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

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
) where


import           Data.Discrimination
import           Data.Discrimination.Grouping
import           Data.Maybe

  
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

instance Show (Elem x xs) where
  show Here = "Here"
  show (There x) = "There (" ++ show x ++ ")"


getElem :: Elem x xs -> AttribList xs -> x
getElem Here      (AttribCons x _)  = x
getElem (There e) (AttribCons _ xs) = getElem e xs 


ahead :: AttribList (a ': as) -> a
ahead (AttribCons x _) = x


atail :: AttribList (a ': as) -> AttribList as
atail (AttribCons _ xs) = xs


-- | this is for convenience
class ToTuple lst where
  type Tuple lst = r | r -> lst 
  toTuple :: AttribList lst -> Tuple lst
  fromTuple :: Tuple lst -> AttribList lst

-- instance ToTuple '[] where
--   type Tuple '[] = ()
--   toTuple _ = ()
--   fromTuple _ = anil

-- instance ToTuple '[a] where
--  type Tuple '[a] = a
--  toTuple (AttribCons x AttribNil) = x
--  fromTuple x = x `acons` anil

instance ToTuple '[a,b] where
  type Tuple '[a,b] = (a,b)
  toTuple (AttribCons x (AttribCons y AttribNil)) = (x,y)
  fromTuple (x,y) = x `acons` (y `acons` anil)

instance ToTuple '[a,b,c] where
  type Tuple '[a,b,c] = (a,b,c)
  toTuple (AttribCons x (AttribCons y (AttribCons z AttribNil))) = (x,y,z)
  fromTuple (x,y,z) = x `acons` (y `acons` (z `acons` anil))

instance ToTuple '[a,b,c,d] where
  type Tuple '[a,b,c,d] = (a,b,c,d)
  toTuple (AttribCons x (AttribCons y (AttribCons z (AttribCons w AttribNil))))
    = (x,y,z,w)
  fromTuple (x,y,z,w) = x `acons` (y `acons` (z `acons` (w `acons` anil)))


acons = AttribCons

infixr 8 `acons`

-- (<&>) = AttribCons

-- infixr 8 <&>

anil = AttribNil

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
