{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}


module Data.Bitree where

import           Control.Applicative
import           Data.Aeson
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Binary
import           Data.Bitraversable
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable
import qualified Data.Tree            as Tr
import           GHC.Generics


-- | n = node, l = leaf
--
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


getRoot1 :: Bitree a a -> a
getRoot1 = either id id . getRoot


getLeaves :: Bitree n l -> [l]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL x) = [x]


getNodes :: Bitree n l -> [n]
getNodes (PN x xs) = x:(concatMap getNodes xs)
getNodes (PL _) = []


toTree :: Bitree a a -> Tr.Tree a
toTree (PN x xs) = Tr.Node x (map toTree xs)
toTree (PL x)    = Tr.Node x []



-- | duplicate of comonad is dual to join of monad, i.e. duplicate :: w a -> w (w a)
--   In Bitree case, we can make a tree where each node is the subtree at the node point using duplicate
duplicate :: Bitree c a -> Bitree (Bitree c a) (Bitree c a)
duplicate (PN x xs) = PN (PN x xs) (map duplicate xs)
duplicate (PL x) = PL (PL x)
