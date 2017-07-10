{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}


module Data.Bitree where

import Control.Applicative
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Binary
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable
import GHC.Generics

-- | chunk = chunktag, token = token in node. 
--   typically token will be (pos = postag, a = content)

data Bitree n l = PN n [Bitree n l]
                | PL l
                deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Bifunctor Bitree where
  bimap f g (PN x xs) = PN (f x) (map (bimap f g) xs)
  bimap f g (PL y)    = PL (g y) 

instance Bifoldable Bitree where
  bifoldMap f g (PN x xs) = f x <> foldMap (bifoldMap f g) xs
  bifoldMap f g (PL y)    = g y
  
instance Bitraversable Bitree where
  bitraverse f g (PN x xs) = PN <$> f x <*> traverse (bitraverse f g) xs
  bitraverse f g (PL y)    = PL <$> g y

instance (FromJSON n, FromJSON l) => FromJSON (Bitree n l) where
  parseJSON = genericParseJSON defaultOptions

instance (ToJSON n, ToJSON l) => ToJSON (Bitree n l) where
  toJSON = genericToJSON defaultOptions

instance (Binary n, Binary l) => Binary (Bitree n l)


getRoot :: Bitree n l -> Either n l
getRoot (PL x)   = Right x
getRoot (PN x _) = Left x


-- | duplicate of comonad is dual to join of monad, i.e. duplicate :: w a -> w (w a)
--   In Bitree case, we can make a tree where each node is the subtree at the node point using duplicate 
duplicate :: Bitree c a -> Bitree (Bitree c a) (Bitree c a)
duplicate (PN x xs) = PN (PN x xs) (map duplicate xs)
duplicate (PL x) = PL (PL x)

