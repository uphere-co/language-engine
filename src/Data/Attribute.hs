{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Attribute
( anil
, ahead
, atail
, (<&>)
) where


data AttribList (list :: [*]) where
  AttribNull :: AttribList '[]
  AttribCons :: a -> AttribList as -> AttribList (a ': as)


ahead :: AttribList (a ': as) -> a
ahead (AttribCons x _) = x


atail :: AttribList (a ': as) -> AttribList as
atail (AttribCons _ xs) = xs


(<&>) = AttribCons

infixr 8 <&>

anil = AttribNull






  
  

