{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--Uncomment it to run it in REPL
--module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe,catMaybes)
import           System.IO                             (stdin,stdout)
import           Control.Arrow                         ((***))
import           Data.Either                           (rights)
import qualified Data.Text.Lazy                as T.L
import qualified Data.Text.Lazy.IO             as T.L.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util
import           WikiEL.ETL.RDF.Binary
import           WikiEL.ETL.Parser                     (wordnetSynsetYAGO)
import           WikiEL.Type.WordNet                   (SynsetY)

import           WikiEL.ETL.RDF.Yago


-- ========================
import           Data.Word                             (Word32)
import           Foreign.Store

import           System.Environment                    (getArgs)

-- For Wiki interlinks
--import           Data.Text                             (Text)
import           Data.List                             (foldl')
import qualified Data.Map.Strict               as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Fusion.Bundle     as B
import qualified WikiEL.Util.Hash              as H
import qualified WikiEL.Graph                  as G
import qualified WikiEL.ETL.RDF.Binary         as BR

import qualified Data.ByteString.Lazy.Char8    as BL


-- For profiling
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Control.Monad.ST                      (runST)
import qualified Data.Vector.Storable          as S

import           Control.Exception                     (evaluate)
import           Control.DeepSeq                       (force)




type LText = T.L.Text


wikidata :: (ParsingState,Text) -> Text -> IO (ParsingState,Text)
wikidata (!prevState, prevPartialBlock) block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    rs    = map readlineWikidata lines
    (state,ts)    = flattenStatementStream prevState rs
  --mapM_ print rs
  mapM_ print ts
  return (state,partialBlock)
  --mapM_ T.IO.putStrLn rs


main2 = readBlocks stdin wikidata (initState, "")


type HashInvs =  M.Map H.WordHash Text
data Foo = Foo { _edges :: UV.Vector (H.WordHash, H.WordHash)
               , _names  :: HashInvs
               }
         deriving (Show)

initFoo = Foo UV.empty M.empty

printFoo foo@(Foo edges names) = do
  let 
    lookup key = M.lookup key names
  mapM_ (print . (lookup *** lookup)) (UV.toList edges)
  print names

tryAdd :: HashInvs -> Text -> HashInvs
tryAdd invs word = M.insert (H.wordHash word) word invs

parseInterlinks :: Text -> (Text, Text)
parseInterlinks line = (from, to)
  where
    [from,to] = T.words line

-- UV.snoc is very inefficient for large Foo
addEdge :: Foo -> (Text,Text) -> Foo
addEdge foo@(Foo edges names) (from,to) = foo'
  where
    edge = (H.wordHash from, H.wordHash to)
    foo' = Foo (UV.snoc edges edge) (tryAdd (tryAdd names from) to)

loadEdges :: [Text] -> Foo
loadEdges lines  = Foo edges names
  where
    es = map parseInterlinks lines
    edges = UV.fromList (map (H.wordHash *** H.wordHash) es)
    f accum (from, to) = tryAdd (tryAdd accum from) to
    names = foldl' f M.empty es

showPath :: HashInvs -> UV.Vector H.WordHash -> [ Text]
showPath invs path = catMaybes (UV.foldl' f [] path)
  where
    f accum hash = M.lookup hash invs : accum

showPaths :: Foldable t => HashInvs -> t (UV.Vector H.WordHash) -> IO ()
showPaths names = mapM_ (print . showPath names)
showPathPairs :: Foldable t => HashInvs -> t (UV.Vector H.WordHash, UV.Vector H.WordHash) -> IO ()
showPathPairs names = mapM_  (print . (\(x,y)-> reverse (showPath names y) ++ tail (showPath names x)))


-- loadInterlinks is uselessly slow for now.
loadInterlinks :: (Foo,Text) -> Text -> IO (Foo,Text)
loadInterlinks (prevState, prevPartialBlock) block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    edges    = map parseInterlinks lines
    state = foldl' addEdge prevState edges
  --mapM_ print edges
  return (state,partialBlock)

foo :: ([Text] -> a) -> FilePath -> IO a
foo f  filepath = do
  content <- T.L.IO.readFile filepath
  let
    lines = map T.L.toStrict (T.L.lines content)
    state = f lines
  return state

data WNTypes = WNTypes { _types  :: !(M.Map H.WordHash [H.WordHash])
                       , _toStr  :: !(M.Map H.WordHash Text)
                       }

instance Show WNTypes where
  show (WNTypes types names) = show (M.size types) ++ " names are mapped." ++ show (M.size names) ++ " hashes."

-- Stack-overflowed version. Left for profiling exercise.
loadWordnetTypes :: [Text] -> WNTypes
loadWordnetTypes lines = foldl' addKey (WNTypes M.empty M.empty) edges
  where
    edges    = map parseInterlinks lines
    addKey foo@(WNTypes types names) edge = WNTypes (f types edge) (g names edge)
      where
        hash = H.wordHash
        tryAppend invs (key, val) = M.insertWith (++) key [val] invs
        trySet    invs word = M.insert (H.wordHash word) word invs    
        f ts (entity, synset) = tryAppend ts (hash entity, hash synset)    
        g ns (entity, synset) = trySet (trySet ns entity) synset


wordnetType :: WNTypes -> Text -> [Text]
wordnetType table@(WNTypes types names) name = f ts
  where
    key = H.wordHash name
    f Nothing = []
    f (Just ts) = mapMaybe (\t -> M.lookup t names) ts
    ts = M.lookup key types

main4 = do
  args <- getArgs 
  wn <- foo loadWordnetTypes (args !! 0) -- "/scratch/wavewave/test/wnTypes.1M" -- "enwiki/wnTypes"
  print wn

{-
--For preparing test data:
$ lbzcat yago/yago3_entire_tsv.bz2 | grep "<linksTo>" > yago/wikilinks
$ time cat yago/wikilinks | runhaskell -i./src/ test/testApp.hs > enwiki/interlinks
real	60m40.005s

-- using `yago-bin` with `wordnetTypes`
cabal build yago-bin --builddir=../dists/wiki-ner
$ time grep subclassOf yago/wordnet | ../dists/wiki-ner/build/yago-bin/yago-bin > enwiki/wnTypes
-- using `yago-bin` with `wordnetTaxonomy`
cabal build yago-bin --builddir=../dists/wiki-ner
$ time grep subclassOf yago/wordnet | ../dists/wiki-ner/build/yago-bin/yago-bin > enwiki/taxonomies
real	0m3.048s

cp enwiki/wnTypes enwiki/synsets
cat enwiki/taxonomies >> enwiki/synsets

-}



{--------remove this
edgeOrdering :: Ord a => ((a,a)->a) -> (a,a) -> (a,a) -> Ordering
edgeOrdering direction left right = compare (direction left) (direction right)

fff, ttt :: Ord a => (a,a)->a
fff  (x,_) = x
ttt  (_,x) = x

tt,ff :: Ord a => (a,a) -> (a,a) -> Ordering
ff = edgeOrdering fff
tt = edgeOrdering ttt

-- 17s for 1M vs 12s for sortEdges
sortEdges2 G.From edges = (G.From, UV.modify (sortBy ff) edges)
sortEdges2 G.To   edges = (G.To,   UV.modify (sortBy tt) edges)
-}


test1 :: (G.Direction, UV.Vector (H.WordHash, H.WordHash)) -> HashInvs -> IO ()
test1 sorted@(d,edges) names = do
  let
    dEdges  = G.neighbor sorted
    tmp = G.allPathsUpto dEdges 1079244021 3
    tmp2 = G.allPathsUpto dEdges (H.wordHash "Diemelsee") 3
    --tmp3 = G.allPathsUpto dEdges (H.wordHash "Sundar_Pichai") 3
    tmp3 = G.allPathsUpto dEdges (H.wordHash "Larry_Page") 3
    
    fNode node cutoff = G.accumReachable (UV.fromList [(node,0)]) cutoff (G.neighbor sorted) (UV.fromList [node],0)
    t1 = fNode (H.wordHash "Larry_Page") 3
    t2 = fNode (H.wordHash "Steve_Jobs") 3

      --print "=================================="
  mapM_ print (G.neighborOverlap t1 t2)
  -- print $ B.length tmp
  -- print $ B.length tmp2
  -- print $ B.length tmp3
  --mapM_ (print . showPath names) (B.toList tmp)
  --mapM_ (print . showPath names) (B.toList tmp2)
  -- mapM_ (print . showPath names) (take 100 (B.toList tmp3))
  --print $ dEdges 1079244021
  --print $ dEdges (H.wordHash "Germany")

main3init = do
  cc@(Foo edges names) <- foo loadEdges "enwiki/interlinks" -- ~16min to sort
  wn <- foo loadWordnetTypes "enwiki/synsets"
  taxons@(Foo tes tns) <- foo loadEdges "enwiki/synsets" -- ~16min to sort
  let
    sorted = G.sortEdges G.From  edges
    sortedTEs = G.sortEdges G.From  tes
  print $ UV.length edges
  print $ M.size names
  print $ UV.length (snd sorted)

  print $ M.size tns
  print $ UV.length (snd sortedTEs)

  print $ wn
  
  store <- newStore cc
  store2 <- newStore sorted
  store3 <- newStore wn
  store4 <- newStore taxons
  store5 <- newStore sortedTEs
  print store
  print store2
  print store3
  print store4
  print store5

{-
-- Script for testing in REPL

idx=2
idx2=1
Just store <- lookupStore idx :: IO (Maybe (Store Foo))
Just store2 <- lookupStore idx2 :: IO (Maybe (Store (G.Direction, UV.Vector (H.WordHash, H.WordHash))))
cc@(Foo edges names) <- readStore store
sorted@(d,es) <- readStore store2

idx3=3
Just store3 <- lookupStore idx :: IO (Maybe (Store WNTypes))
wn@(WNTypes wes wns) <- readStore store3

idx4=4
idx5=5
Just store4 <- lookupStore idx4 :: IO (Maybe (Store Foo))
Just store5 <- lookupStore idx5 :: IO (Maybe (Store (G.Direction, UV.Vector (H.WordHash, H.WordHash))))
taxons@(Foo tes tns) <- readStore store4
sortedTEs <- readStore store5


hash word = H.wordHash (T.pack word)

-- store1~3 takes about 26.5% of memory ~ 34 GB.


showPathPairs  names $ G.destOverlapUpto dfe 2 (hash "Larry_Page") (hash "Steve_Jobs")



--test1 sorted names
fNode node cutoff = G.accumReachable (UV.fromList [(node,0)]) cutoff (G.neighbor sorted) (UV.fromList [node],0)
t1 = fNode (hash "Larry_Page") 2
t2 = fNode (hash "Steve_Jobs") 2
UV.length t1
UV.length t2
a = G.neighborOverlap t1 t2
length a

aa = map (\((lh,ld),(rh,rd)) -> (M.lookup lh names, ld, rd)) a
mapM_ print (filter (\(_,ld,rd) -> ld<3 && rd<2) aa)

pNode node cutoff = G.allPathsUpto (G.neighbor sorted) (hash node) cutoff

--paths = G.destOverlap (pNode "Larry_Page" 2) (pNode "Steve_Jobs" 2)
dfe = G.neighbor sorted
paths = G.destOverlapUpto dfe 2 (hash "Larry_Page") (hash "Steve_Jobs")

showPaths names paths = mapM_ print (map (showPaths names) paths)



-- With
-- hash word = H.wordHash (T.pack word)
-- wn@(WNTypes wes wns) <- foo loadWordnetTypes2 "enwiki/wnTypes"
taxons@(Foo tes tns) <- foo loadEdges "enwiki/taxonomies"
synset node cutoff = G.allPathsUpto (G.neighbor (G.sortEdges G.From tes)) (hash node) cutoff

synsetPath node1 node2 cutoff = G.destOverlapUpto (G.neighbor (G.sortEdges G.From tes)) cutoff (hash node1) (hash node2)
synsetPath2 node1 node2 cutoff = G.destOverlapUpto (G.neighbor (G.sortEdges G.To tes)) cutoff (hash node1) (hash node2)

showPaths wns $ synset "football_103378765" 2
showPathPairs wns $synsetPath "baseball_100471613" "abstraction_100002137" 15
showPathPairs wns $synsetPath2 "contact_sport_100433458" "field_game_100467719" 10
-}

main3 :: Word32 -> Word32 -> IO ()
main3 idx idx2 = do
  Just store <- lookupStore idx :: IO (Maybe (Store Foo))
  Just store2 <- lookupStore idx2 :: IO (Maybe (Store (G.Direction, UV.Vector (H.WordHash, H.WordHash))))
  cc@(Foo edges names) <- readStore store
  sorted@(d,es) <- readStore store2
  print $ UV.length edges
  print $ M.size names  
  print $ UV.length es

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
  print $ M.size (M.fromList ts)

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
  

prof3 = do
  let
    hash = H.wordHash
    filepath = "enwiki/interlinks.1M"
  
  -- Check that "a list of inverse hash has expected memory footprint".
  content <- T.L.IO.readFile filepath
  let
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
    f accum (from, to) = tryAdd (tryAdd accum from) to
    names = foldl' f M.empty es
    names' = M.fromList (concatMap (\(x,y) -> [(hash x, x),(hash y,y)]) es)
  print $ M.size names
  -- (3.09 secs, 2,598,081,592 bytes)
  print $ M.size names'
  -- (3.57 secs, 2,790,087,032 bytes)

  
main :: IO ()
main = prof2

