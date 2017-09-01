{-# LANGUAGE OverloadedStrings
           , BangPatterns
 #-}


module Main where
--Uncomment it to run it in REPL
--module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe,catMaybes)
import           System.IO                             (stdin,stdout)
import           Control.Arrow                         (second,(***))
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
--import qualified WikiEL.Util.Hash              as H
import qualified Graph.Internal.Hash           as H
import qualified WikiEL.Graph                  as G
import qualified WikiEL.ETL.RDF.Binary         as BR
import qualified Data.ByteString.Lazy.Char8    as BL

import qualified Graph.ETL                     as G.E
import qualified WikiEL                        as WEL
import           Test.Data.Filename

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

parseInterlinks2 line = second T.tail (T.break (=='\t') line)

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
  content <- T.IO.readFile filepath
  return $ f (T.lines content)
  {-
  content <- T.L.IO.readFile filepath
  let
    lines = map T.L.toStrict (T.L.lines content)
    state = f lines
  return state
-}

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
  cc@(G.E.Graph edges names) <- G.E.applyLines G.E.loadGraph "enwiki/edges" -- ~40min to sort in REPL. ~10min with a compiled binary.
  --cc@(G.E.Graph edges names) <- G.E.applyLines G.E.loadGraph "enwiki/wnTypes.1M"
  --wn <- foo loadWordnetTypes "enwiki/synsets"
  --taxons@(Foo tes tns) <- foo loadEdges "enwiki/synsets" -- ~16min to sort
  let
    sorted = G.sortEdges G.From  edges
    --sortedTEs = G.sortEdges G.From  tes
  print $ UV.length edges
  print $ M.size names
  print $ UV.length (snd sorted)
  store <- newStore cc
  store1 <- newStore sorted
  
  tagger <- WEL.loadFEMtagger reprFile classFiles  
  a3 <- loadNEROutfile nerNewsFile3 posNewsFile3
  a4 <- loadNEROutfile nerNewsFile4 posNewsFile4
  a5 <- loadNEROutfile nerNewsFile5 posNewsFile5  
  let
    mentions3 = tagger a3
    mentions4 = tagger a4
    mentions5 = tagger a5
  print $ length mentions3
  print $ length mentions4
  print $ length mentions5
  store2 <- newStore tagger
  store3 <- newStore mentions3
  store4 <- newStore mentions4
  store5 <- newStore mentions5
    
  
  print store
  print store1
  print store2
  print store3
  print store4
  print store5
  --print $ M.size tns
  --print $ UV.length (snd sortedTEs)
  --print $ wn  
  --store3 <- newStore wn
  --store4 <- newStore taxons
  --store5 <- newStore sortedTEs  
  --print store3
  --print store4
  --print store5

type Node = H.WordHash
type NodesSim = Node -> Node -> Int

-- n : Node, s : Score type
mostSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> n -> [n] -> Maybe (s,n,n)
mostSimilar f ref ns = fCutoff maxsim
  where
    maxsim = maximum $ map (\n -> (f ref n, ref, n)) ns
    fCutoff sim@(score,_,_) | score>5 = Just sim
    fCutoff _ = Nothing

matchToSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> [n] -> [n] -> Maybe (s,n,n)
matchToSimilar f refs ns = mayMax ss
  where
    ss = mapMaybe (\ref -> mostSimilar f ref ns) refs
    mayMax [] = Nothing
    mayMax vs = Just (maximum vs)

{-
-- Script for testing in REPL
import           Main
import           Data.Maybe                            (mapMaybe,catMaybes)
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import qualified Data.Vector.Unboxed           as UV
import           Foreign.Store
import qualified Graph.ETL                     as G.E
import qualified WikiEL.Graph                  as G
import qualified Graph.Internal.Hash           as H

import qualified WikiEL                        as WEL
import           Test.Data.Filename

hash word = H.wordHash (T.pack word)
showPath invs path = catMaybes (UV.foldl' f [] path) where f accum hash = M.lookup hash invs : accum
showPathPairs names = mapM_  (print . (\(x,y)-> reverse (showPath names y) ++ tail (showPath names x)))

idx =0
idx1=1
idx2=2
Just store <- lookupStore idx :: IO (Maybe (Store G.E.Graph))
Just store1 <- lookupStore idx1 :: IO (Maybe (Store (G.Direction, UV.Vector (H.WordHash, H.WordHash))))
cc@(G.E.Graph edges names) <- readStore store
sorted@(d,es) <- readStore store1

Just store2 <- lookupStore idx2 :: IO (Maybe (Store ([(T.Text, N.NamedEntityClass, POS.POSTag)]-> [EL.EntityMention T.Text])))
tagger <- readStore store2

Just s3 <- lookupStore 3 :: IO (Maybe (Store ([EL.EntityMention T.Text])))
Just s4 <- lookupStore 4 :: IO (Maybe (Store ([EL.EntityMention T.Text])))
Just s5 <- lookupStore 5 :: IO (Maybe (Store ([EL.EntityMention T.Text])))
mentions3 <- readStore s3
mentions4 <- readStore s4
mentions5 <- readStore s5

mapM_ print mentions3
mapM_ print mentions4
mapM_ print mentions5

paths len wp1 wp2 = G.destOverlapUpto (G.neighbor sorted) len (hash wp1) (hash wp2)
f x y = length (paths 1 x y)

mostSimilar f "Nike,_Inc." ["Michael_Jordan", "Michael_Jordan_(mycologist)", "Michael_Jordan_(Irish_politician)"]
matchToSimilar f ["Nike,_Inc.","United_States"] ["Michael_Jordan", "Michael_Jordan_(mycologist)", "Michael_Jordan_(Irish_politician)"]

showPathPairs names $ paths 1 "Michael_Jordan" "United_States"
showPathPairs names $ paths 1 "Michael_Jordan_(mycologist)" "United_States"
showPathPairs names $ paths 1 "Michael_Jordan_(Irish_politician)" "United_States"

showPathPairs names $ paths 2 "Michael_Jordan" "Nike,_Inc."
showPathPairs names $ paths 2 "Michael_Jordan_(mycologist)" "Nike,_Inc."
showPathPairs names $ paths 2 "Michael_Jordan_(Irish_politician)" "Nike,_Inc."

length $ paths 1 "Michael_Jordan" "Nike,_Inc."
length $ paths 1 "Michael_Jordan_(mycologist)" "Nike,_Inc."
length $ paths 1 "Michael_Jordan_(Irish_politician)" "Nike,_Inc."

length $ paths 2 "Michael_Jordan" "Nike,_Inc."
length $ paths 2 "Michael_Jordan_(mycologist)" "Nike,_Inc."
length $ paths 2 "Michael_Jordan_(Irish_politician)" "Nike,_Inc."





idx3=3
Just store3 <- lookupStore idx :: IO (Maybe (Store WNTypes))
wn@(WNTypes wes wns) <- readStore store3

idx4=4
idx5=5
Just store4 <- lookupStore idx4 :: IO (Maybe (Store Foo))
Just store5 <- lookupStore idx5 :: IO (Maybe (Store (G.Direction, UV.Vector (H.WordHash, H.WordHash))))
taxons@(Foo tes tns) <- readStore store4
sortedTEs <- readStore store5


-- store1~3 takes about 26.5% of memory ~ 34 GB.
showPathPairs names $ G.destOverlapUpto dfe 2 (hash "Larry_Page") (hash "Steve_Jobs")



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

main4 = do
  args <- getArgs 
  wn <- foo loadWordnetTypes (args !! 0) -- "/scratch/wavewave/test/wnTypes.1M" -- "enwiki/wnTypes"
  print wn

--compare Text vs ByteSTring
main5 = do
  cc@(Foo edges names) <- foo loadEdges "enwiki/interlinks.10M"
  --(0.05 secs, 781,992 bytes)
  --(1.62 secs, 858,703,296 bytes)     --with strict Text
  print $ UV.length edges
  --(27.39 secs, 38,171,046,944 bytes)
  --(34.03 secs, 41,512,027,152 bytes) --with strict Text and parserlink2
  --(21.60 secs, 24,037,759,056 bytes) --with strict Text
  print $ M.size names
  --(50.94 secs, 28,697,355,912 bytes)
  --(36.51 secs, 28,697,355,544 bytes) --with strict Text

  gg@(G.E.Graph es ns) <- G.E.applyLines G.E.loadGraph "enwiki/interlinks.10M"
  --(0.11 secs, 362,147,360 bytes)
  print $ UV.length es
  --(30.99 secs, 22,382,063,760 bytes) -- With Maybe
  --(27.19 secs, 21,342,065,888 bytes) -- Without Maybe
  --(17.40 secs, 21,087,622,824 bytes) -- Without Maybe and with toLink2
  print $ M.size ns
  --(34.97 secs, 25,536,287,552 bytes) -- With Maybe
  --(33.73 secs, 25,536,287,552 bytes) -- Without Maybe


  
main :: IO ()
--main = main5
main = main3init

