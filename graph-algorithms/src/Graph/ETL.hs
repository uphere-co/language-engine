{-# LANGUAGE BangPatterns #-}

module Graph.ETL
  ( module Graph.ETL
  ) where

import           Control.Arrow                        ((***))
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Vector.Unboxed           as UV
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BS.L
import qualified Data.ByteString.Internal      as BSI (c2w)

import qualified Graph.Internal.Hash              as H

type BString  = BS.ByteString
type BStringL = BS.L.ByteString

data Graph = Graph { _edges :: UV.Vector (H.WordHash, H.WordHash)
                   , _names  :: M.Map H.WordHash BString
                   }
             deriving (Show)

applyLines :: ([BString] -> a) -> FilePath -> IO a
applyLines f filepath = do
  content <- BS.readFile filepath
  let
    nl = BSI.c2w '\n'
    g c | BS.null c = []
    g c =  BS.split nl (BS.init c)
    ls = g content
  return $ f ls

loadGraph :: [BString] -> Graph
loadGraph ls  = Graph edges names
  where
    tab = BSI.c2w '\t'
    --toLink line = second BS.tail (BS.break (== tab) line) -- takes ~60% more time
    toLink l = (x,y)
      where [x,y] = BS.split tab l
    lnks = map toLink ls
    edges = UV.fromList (map (H.hash *** H.hash) lnks)
    --tryAdd invs word = h `seq` M.insert h word invs  -- slightly slower and uses a bit more memory
    --  where
    --    h=hash word
    trySet invs word = M.insert (H.hash word) word invs
    f accum (from, to) = trySet (trySet accum from) to
    names = L.foldl' f M.empty lnks
