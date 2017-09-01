module Data.BitreeZipper.Util where

import           Control.Monad.Loops (iterateUntilM)
import           Data.List           (unfoldr)
--
import           Data.Bitree         (Bitree)
import           Data.BitreeZipper   (BitreeZipper,current,parent)


firstSiblingBy :: (BitreeZipper c t -> Maybe (BitreeZipper c t))
               -> (Bitree c t -> Bool)
               -> BitreeZipper c t
               -> Maybe (BitreeZipper c t)
firstSiblingBy dir p x = iterateUntilM (p.current) dir x



siblingsBy :: (BitreeZipper c t -> Maybe (BitreeZipper c t))
           -> (Bitree c t -> Bool)
           -> BitreeZipper c t
           -> [BitreeZipper c t]
siblingsBy dir p = filter (p.current) . unfoldr (\z -> dir z >>= \z' -> return (z',z'))




