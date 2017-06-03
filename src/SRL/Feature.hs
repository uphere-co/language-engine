{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature where

import           Control.Lens            hiding (levels)
import           Control.Monad                  ((<=<),when)
import           Data.Bifunctor                 (bimap)
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy,zip4)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text               as T   (intercalate,unpack)
-- import qualified Data.Text.Format        as TF
import qualified Data.Text.IO            as TIO
-- import qualified Data.Text.Lazy          as TL
import           Data.Tree                      (levels)
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple.Convert                      (cutf8)
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           PropBank.Type.Prop
--
import           SRL.PropBankMatch
import           SRL.Util
--
-- import Debug.Trace


data Position = Before | After | Embed
              deriving (Show,Eq,Ord)

data Direction = Up | Down
               deriving (Show,Eq,Ord)

type ParseTreePath = [(Either ChunkTag POSTag, Direction)]
                        
phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)

position :: Int ->  PennTreeGen c (Int,(p,a)) -> Position
position n tr = let (b,e) = termRange tr
                in if | n < b     -> Before
                      | n > e     -> After
                      | otherwise -> Embed


elimCommonHead :: [PennTreeIdxG c (p,a)]
               -> [PennTreeIdxG c (p,a)]
               -> (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
elimCommonHead lst1 lst2 = go lst1 lst2
  where
    range = fst . phraseType 
    go (x0:x1:xs) (y0:y1:ys) 
      | range x0 == range y0 && range x1 == range y1 = go (x1:xs) (y1:ys)
      | range x0 == range y0 && range x1 /= range y1 = (Just x0,x1:xs,y1:ys)
      | otherwise = (Nothing,x0:x1:xs,y0:y1:ys)
    go (x0:[]) (y0:ys)
      | range x0 == range y0 = (Just x0,[],ys)
      | otherwise = (Nothing,[x0],y0:ys)
    go (x0:xs) (y0:[])
      | range x0 == range y0 = (Just x0,xs,[])
      | otherwise = (Nothing,x0:xs,[y0])
    go []     ys     = (Nothing,[],ys)
    go xs     []     = (Nothing,xs,[])


parseTreePathFull :: (Int,Range)
                  -> PennTreeIdxG c (p,a)
                  -> (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
parseTreePathFull (start,target) tr = elimCommonHead (contain start tr) (containR target tr)

parseTreePath :: (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
              -> [(Either c p,Direction)]
parseTreePath (mh,tostart,totarget) =
  case mh of
    Nothing -> []
    Just h -> let lst1 = ((snd.phraseType) h,Down):map ((,Down).snd.phraseType) totarget
                  lst2 = map ((,Up).snd.phraseType) . reverse $ tostart
              in lst2 ++ lst1



simplifyPTP :: (Eq c,Eq p) => [(Either c p, Direction)] -> [(Either c p, Direction)]
simplifyPTP xs = map head (group xs)

annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Int,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))

headWordTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe Int,t))
headWordTree (Dependency root nods edgs') tr =
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
      searchtree = head (dfs (buildG bnds edgs) [root])
      levelMap = IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]
  in fmap (annotateLevel levelMap) tr 

headWord :: PennTreeIdxG ChunkTag (Maybe Int,(POSTag,Text)) -> Maybe (Int,Text)
headWord  = safeHead . sortBy (compare `on` fst)
          . mapMaybe (\(_,(ml,(_,t))) -> (,) <$> ml <*> pure t) . getLeaves 

lemmatize :: IntMap Text
          -> PennTreeIdxG ChunkTag (POSTag,Text)
          -> PennTreeIdxG ChunkTag (POSTag,(Text,Text))
lemmatize m = bimap id (\(i,(p,x)) -> (i,(p,(x,fromJust (IM.lookup i m)))))


type TreeICP a = Tree (Range,ChunkTag) (Int,(POSTag,a))

type TreeZipperICP a = TreeZipper (Range,ChunkTag) (Int,(POSTag,a))

isVBN :: TreeZipperICP a -> Bool
isVBN z = case current z of
            PL (_,(p,_)) -> p == VBN
            _            -> False 

withCopula :: TreeZipperICP (Text,Text) -> Bool
withCopula z = case current <$> (prev <=< parent) z of
                 Just (PL (_,(_,(_,l)))) -> l == "be"
                 _                       -> False

isInNP :: TreeZipperICP (Text,Text) -> Bool
isInNP z = case current <$> (parent <=< parent) z of
             Just (PN (_,c) _) -> c == NP
             _             -> False


isInPP :: TreeZipperICP (Text,Text) -> Bool
isInPP z = case current <$> (parent z) of
             Just (PN (_,c) _) -> c == PP
             _                 -> False

isPassive :: TreeZipperICP (Text,Text) -> Bool
isPassive z = let b1 = isVBN z
                  b2 = withCopula z
                  b3 = isInNP z
                  b4 = isInPP z
              in (b1 && b2) || (b1 && b3) || (b1 && b4)

{- 
findHeadNode :: [Range] -> PennTreeIdxG ChunkTag (Maybe Int,(POSTag,Text)) -> [Range]
findHeadNode rngs headwordtree =
  let headranges :: [(Range, (Int,Text))]
      headranges = mapMaybe (\rng -> fmap (rng,) . headWord =<< matchR rng headwordtree) rngs
  in map (^._1) $ sortBy (compare `on` (^. _2 . _1)) headranges
-}

  
featuresForArgNode :: SentenceInfo -> Int -> Argument -> MatchedArgNode
                   -> [(Range,ParseTreePath,Maybe (Int,Text))]
featuresForArgNode sentinfo predidx arg node =
  let rngs = node ^.. mn_trees . traverse . _1
      ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
      opaths = map parseTreePath parsetrees
      paths = map (simplifyPTP . parseTreePath) parsetrees
      headwordtrees = headWordTree dep ipt
      heads = map (\rng -> headWord =<< matchR rng headwordtrees) rngs
      comparef Nothing  _        = LT
      comparef _        Nothing  = GT
      comparef (Just x) (Just y) = (compare `on` (^._1)) x y
  in  sortBy (comparef `on` (view _3)) $ zip3 rngs paths heads        


type ArgNodeFeature = (Text,(Range,ParseTreePath,Maybe (Int,Text)))
      
featuresForArg :: SentenceInfo -> Int -> MatchedArgument -> [ArgNodeFeature]
featuresForArg sentinfo predidx arg =
  flip mapMaybe (arg^.ma_nodes) $ \node -> do
    let label = arg^.ma_argument.arg_label
    fs <- safeHead (featuresForArgNode sentinfo predidx (arg^.ma_argument) node)
    return (label,fs)


type InstanceFeature = (Int,Text,Maybe Voice, [[ArgNodeFeature]])
  
featuresForInstance :: SentenceInfo -> IntMap (Text,Voice) -> MatchedInstance -> InstanceFeature
featuresForInstance sentinfo voicemap inst = 
  let predidx = findRelNode (inst^.mi_arguments)
      rolesetid = inst^.mi_instance.inst_lemma_roleset_id
      argfeatures = map (featuresForArg sentinfo predidx) . filter ((/= "rel") . (^.ma_argument.arg_label)) $ inst^.mi_arguments
      voicefeature = fmap snd (IM.lookup predidx voicemap)
  in (predidx,rolesetid,voicefeature,argfeatures)




data Voice = Active | Passive deriving Show

formatVoice :: Maybe Voice -> String
formatVoice Nothing = " "
formatVoice (Just Active) = "active"
formatVoice (Just Passive) = "passive"

formatPTP :: ParseTreePath -> String
formatPTP = foldMap f 
  where
    f (Left  c,Up  ) = show c ++ "↑"
    f (Left  c,Down) = show c ++ "↓"
    f (Right p,Up  ) = show p ++ "↑"
    f (Right p,Down) = show p ++ "↓"


formatArgNodeFeature :: ArgNodeFeature -> String
formatArgNodeFeature (label,(rng,ptp,mhead)) =
    printf "%10s %10s %30s %s" (T.unpack label) (show rng) (formatPTP ptp) (hstr mhead)
  where
    hstr Nothing = ""
    hstr (Just (_,w)) = (T.unpack w)

--   (Text,(Range,ParseTreePath,Maybe (Int,Text)))



formatInstanceFeature :: InstanceFeature -> String
formatInstanceFeature (predidx,rolesetid,voicefeature,argfeatures) =
  let fs = concat argfeatures
  in foldMap (\x -> printf "%10s %20s %10s %s\n" (show predidx) (T.unpack rolesetid) (formatVoice voicefeature) (formatArgNodeFeature x)) fs


voice :: (PennTree,S.Sentence) -> [(Int,(Text,Voice))]
voice (pt,sent) = 
  let ipt = mkPennTreeIdx pt
      lemmamap =  foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> Just (n,(txt,if isPassive z then Passive else Active))
                  _ -> Nothing
  in mapMaybe testf $ toList (mkTreeZipper [] lemmapt)



features :: (Int,SentenceInfo,[Instance]) -> IO ()
features (_i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
      vmap = IM.fromList $ voice (pt,sentinfo^.corenlp_sent)
      ifeats = map (featuresForInstance sentinfo vmap) insts
  mapM_ (putStrLn . formatInstanceFeature) ifeats
  -- mapM_ (print . 
  {- putStrLn "voice"
  print $ 
  putStrLn "end voice" -}
