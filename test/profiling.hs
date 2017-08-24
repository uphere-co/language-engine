-- For profiling. Run in testRun context :
-- $ cabal repl testRun
-- > :l test/profiling.hs
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Control.Monad.ST                      (runST)
import           Control.Exception                     (evaluate)
import           Control.DeepSeq                       (force)
import qualified Data.List                     as L
import qualified Data.Vector.Storable          as S
import qualified Data.Map.Strict               as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Text.Lazy                as T.L
import qualified Data.Text.Lazy.IO             as T.L.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO


import qualified WikiEL.Util.Hash              as H
import qualified WikiEL.Graph                  as G



sortV :: (Ord a) => V.Vector a -> V.Vector a
sortV vec = runST $ do
  mvec <- V.unsafeThaw vec
  sort mvec
  V.unsafeFreeze mvec


sortUV :: (Ord a, UV.Unbox a) => UV.Vector a -> UV.Vector a
sortUV vec = runST $ do
  mvec <- UV.unsafeThaw vec
  sort mvec
  UV.unsafeFreeze mvec

sortS :: (Ord a, S.Storable a) => S.Vector a -> S.Vector a
sortS vec = runST $ do
  mvec <- S.unsafeThaw vec
  sort mvec
  S.unsafeFreeze mvec


prof1 :: (S.Storable a, UV.Unbox a) => [a] -> IO ()
prof1 ts = do 
  print $ S.length (S.fromList ts)
  print $ UV.length (UV.fromList ts)
  print $ V.length (V.fromList ts)
  --print $ M.size (M.fromList ts)

data IntPair = IP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
             deriving (Show, Eq, Ord)

prof2 = do
  -- Check that "Unboxed vector contains unpacked element and expensive to construct than boxed one"
  let
    ts = take 10000000 (zip ([1..] :: [Int]) ([1..] :: [Int]))
    tsu = map (uncurry IP) ts
    v  = UV.fromList ts
    v2 = V.fromList tsu
    v3 = V.fromList tsu
    v4 =  V.fromList ts
  
  print $ length ts
  -- (2.19 secs, 2,960,766,296 bytes)
  print $ length tsu
  --(2.11 secs, 880,762,584 bytes)
  print $ UV.length v
  --(1.88 secs, 4,537,686,600 bytes)
  print $ V.length v2
  --(0.35 secs, 269,462,184 bytes)
  print $ V.length v3
  --(0.31 secs, 269,462,184 bytes)
  print $ V.length v4
  --(0.35 secs, 269,462,184 bytes)

  vv  <- UV.thaw v
  --(0.08 secs, 160,758,456 bytes)
  vv'  <- UV.thaw v
  --(0.08 secs, 160,757,824 bytes)
  vv3  <- V.thaw v3
  --(0.10 secs, 80,830,960 bytes)
  vv3'  <- V.thaw v3
  --(0.14 secs, 80,834,952 bytes)
  vv4  <- V.thaw v4
  --(0.14 secs, 80,831,704 bytes)
  vv4'  <- V.thaw v4
  --(0.15 secs, 80,831,912 bytes)

  -- Check that hand-written comparison operators are 2x slower than GHC's
  -- with a unboxed vector of 10M 2-tuples.
  print $ (UV.length . snd) (G.sortEdges G.From  v)
  --(103.47 secs, 158,315,091,784 bytes) 
  print $ UV.length (sortUV v)
  --(59.43 secs, 146,395,085,480 bytes)

 

  -- Evidence for "No meaningful difference between Storable and Unbox"
  let
    ts1 = take 1000000 ([1..] :: [Int])
    uv  = UV.fromList ts1
    sv  = S.fromList ts1  
  print $ S.length  sv
  --(0.08 secs, 185,541,288 bytes)
  print $ UV.length uv
  --(0.05 secs, 161,557,976 bytes)
  print $ S.length (sortS  sv)
  --(1.65 secs, 6,480,049,592 bytes)
  print $ UV.length (sortUV uv)
  --(1.43 secs, 5,866,418,624 bytes)
  sv1 <- S.thaw sv
  --(0.00 secs, 8,753,384 bytes)
  uv1 <- UV.thaw uv
  --(0.00 secs, 8,756,520 bytes)
  print "End."

prof3 = do
  let
    hash = H.wordHash
    filepath = "enwiki/interlinks.1M"
  
  -- Check that "a list of inverse hash has expected memory footprint".
  content <- T.L.IO.readFile filepath
  let
    parseInterlinks line = (from, to)
      where
        [from,to] = T.words line
    lines = map T.L.toStrict (T.L.lines content)
    es    = map parseInterlinks lines
    invs2 = map (\(x,y) -> (hash x, x,hash y,y)) es
    tokens = concatMap T.words lines
    invs = concatMap (\(x,y) -> [(hash x, x),(hash y,y)]) es
    tt = map (\x -> (hash x, x)) tokens
  print $ length lines
  -- (0.69 secs, 1,364,017,784 bytes)
  print $ length es
  -- (0.16 secs, 88,764,080 bytes)
  print $ length invs2
  -- (0.07 secs, 88,760,392 bytes) -- low numbers because es and invs2 shares elements
  print $ length tokens
  -- (0.49 secs, 544,235,528 bytes)
  print $ length tt
  -- (0.05 secs, 176,761,656 bytes) -- roughly, this is a size of the inverse hash map
  print $ length invs
  -- (1.21 secs, 616,762,544 bytes) -- this includes temporal variables.

  -- Evidence of "Vector is cheaper to construct than map. (However, it becomes more expensive to get sorted vectors)."
  let
    ns = M.fromList invs
    ns2 = V.fromList invs
    ns' = M.fromList invs
    ns2' = V.fromList invs
    aa = M.toList ns
  print $ M.size ns
  -- 420509
  -- (4.87 secs, 2,957,556,664 bytes)
  print $ M.size $M.fromList $ M.toList ns
  -- (0.09 secs, 71,408,432 bytes) -- overhead of Map structure.
  print $ V.length ns2
  -- 2000000
  -- (0.08 secs, 34,348,264 bytes)
  print $ M.size ns'
  -- (2.09 secs, 1,651,169,640 bytes)
  print $ V.length ns2'
  -- (0.10 secs, 34,348,056 bytes)
  print $ length aa
  -- (0.01 secs, 34,403,696 bytes)
  print $ V.length $ V.fromList aa
  -- (0.02 secs, 9,158,896 bytes)
  
  -- Evidence of "foldl' is a bit faster than M.fromList with concatMap"
  let
    tryAdd invs word = M.insert (H.wordHash word) word invs
    f accum (from, to) = tryAdd (tryAdd accum from) to
    names = L.foldl' f M.empty es
    names' = M.fromList (concatMap (\(x,y) -> [(hash x, x),(hash y,y)]) es)
  print $ M.size names
  -- (3.09 secs, 2,598,081,592 bytes)
  print $ M.size names'
  -- (3.57 secs, 2,790,087,032 bytes)
