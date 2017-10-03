{-# LANGUAGE BangPatterns #-}

module Graph.ETL
  ( module Graph.ETL
  ) where

import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Data.Maybe                            (mapMaybe)
import           Control.Monad.ST                      (runST)
import           Control.Exception                     (evaluate)
import           Control.Arrow                         (second,(***))
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Vector.Unboxed           as UV
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BS.L
import qualified Data.ByteString.Internal      as BSI (c2w, w2c)

import qualified Graph.Internal.Hash              as H

type BString  = BS.ByteString
type BStringL = BS.L.ByteString

data Graph = Graph { _edges :: UV.Vector (H.WordHash, H.WordHash)
                   , _names  :: M.Map H.WordHash BString
                   }
             deriving (Show)

applyLines :: ([BString] -> a) -> FilePath -> IO a
applyLines f filepath = do
  print filepath
  content <- BS.readFile filepath
  let
    nl = BSI.c2w '\n'
    g content | BS.null content = []
    g content =  BS.split nl (BS.init content)
    lines = g content    
  return $ f lines

loadGraph :: [BString] -> Graph
loadGraph lines  = Graph edges names
  where
    tab = BSI.c2w '\t'
    --toLink line = second BS.tail (BS.break (== tab) line) -- takes ~60% more time        
    toLink line = (x,y)
      where [x,y] = BS.split tab line
    links = map toLink lines
    hash = H.hash
    edges = UV.fromList (map (H.hash *** H.hash) links)
    --tryAdd invs word = h `seq` M.insert h word invs  -- slightly slower and uses a bit more memory
    --  where
    --    h=hash word
    trySet invs word = M.insert (H.hash word) word invs
    f accum link@(from, to) = trySet (trySet accum from) to
    names = L.foldl' f M.empty links
