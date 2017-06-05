{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature where

import           Control.Lens            hiding (levels,Level)
import           Control.Monad                  ((<=<),guard,when)
import           Data.Bifunctor                 (bimap)
import           Data.Bifoldable
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy,zip4)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text               as T   (intercalate,unpack)
import qualified Data.Text.IO            as TIO
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
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--

             
phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)

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

annotateLevel :: IntMap Int -> (Int, t) -> (Int,(Maybe Level,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))

headWordTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe Level,t))
headWordTree (Dependency root nods edgs') tr =
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
      searchtree = head (dfs (buildG bnds edgs) [root])
      levelMap = IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]
  in fmap (annotateLevel levelMap) tr 


headWord :: PennTreeIdxG ChunkTag (Maybe Level,(POSTag,Text)) -> Maybe (Int,(Level,(POSTag,Text)))
headWord  = safeHead . sortBy (compare `on` view (_2._1))
          . mapMaybe (\(i,(ml,postxt)) -> fmap (i,) (fmap (,postxt) ml)) . getLeaves 

lemmatize :: IntMap Text
          -> PennTreeIdxG ChunkTag (POSTag,Text)
          -> PennTreeIdxG ChunkTag (POSTag,(Text,Text))
lemmatize m = bimap id (\(i,(p,x)) -> (i,(p,(x,fromJust (IM.lookup i m)))))


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


featuresForArgNode :: SentenceInfo -> Int -> Argument -> MatchedArgNode
                   -> [(Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text))))]
featuresForArgNode sentinfo predidx arg node =
  let rngs = node ^.. mn_trees . traverse . _1
      ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
      opaths = map parseTreePath parsetrees
      paths = map (simplifyPTP . parseTreePath) parsetrees
      headwordtrees = headWordTree dep ipt
      heads = map (\rng -> headWord =<< matchR rng headwordtrees) rngs
      comparef Nothing  _        = GT
      comparef _        Nothing  = LT
      comparef (Just x) (Just y) = (compare `on` view (_2._1)) x y
  in  sortBy (comparef `on` (view _3)) $ zip3 rngs paths heads        
      

featuresForArg :: SentenceInfo -> Int -> MatchedArgument -> [ArgNodeFeature]
featuresForArg sentinfo predidx arg =
  flip mapMaybe (arg^.ma_nodes) $ \node -> do
    let label = arg^.ma_argument.arg_label
    fs <- safeHead (featuresForArgNode sentinfo predidx (arg^.ma_argument) node)
    return (label,fs)


fakeFeaturesForArg :: SentenceInfo -> Int -> Argument -> Range
                       -> (Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text))))
fakeFeaturesForArg sentinfo predidx arg rng =
  let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetree = parseTreePathFull (predidx,rng) ipt
      opath = parseTreePath parsetree
      path = (simplifyPTP . parseTreePath) parsetree
      headwordtrees = headWordTree dep ipt
      hd = headWord =<< matchR rng headwordtrees
      comparef Nothing  _        = GT
      comparef _        Nothing  = LT
      comparef (Just x) (Just y) = (compare `on` view (_2._1)) x y
  in  (rng,path,hd)

    
featuresForInstance :: SentenceInfo -> IntMap (Text,Voice)  -> MatchedInstance -> InstanceFeature
featuresForInstance sentinfo voicemap inst = 
  let predidx = findRelNode (inst^.mi_arguments)
      rolesetid = inst^.mi_instance.inst_lemma_roleset_id
      argfeatures = map (featuresForArg sentinfo predidx) . filter ((/= "rel") . (^.ma_argument.arg_label)) $ inst^.mi_arguments
      voicefeature = fmap snd (IM.lookup predidx voicemap)
  in (predidx,rolesetid,voicefeature,argfeatures)


fakeFeaturesForInstance :: SentenceInfo -> IntMap (Text,Voice) -> MatchedInstance -> InstanceFeature
fakeFeaturesForInstance sentinfo voicemap inst = 
  let predidx = findRelNode (inst^.mi_arguments)
      voicefeature = fmap snd (IM.lookup predidx voicemap)
      rolesetid = inst^.mi_instance.inst_lemma_roleset_id
      args = filter ((/= "rel") . (^.ma_argument.arg_label)) $ inst^.mi_arguments
      ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      argfeatures = do
        arg <- args
        let label = arg^.ma_argument.arg_label
        let rngs = arg ^.. ma_nodes . traverse . mn_node . _1
        guard ((not.null) rngs)
        let rng = (minimum (map (^._1) rngs), maximum (map (^._2) rngs))
            exclst = filter (`isNotOverlappedWith` rng)
                   .  map (\(PN (r,_) _) -> r)
                   .  filter (\case PN _ _ -> True ; _ -> False)
                   . biList . duplicate $ ipt
        rngeach <- exclst
        guard (position predidx rngeach /= Embed)
        return [(label,fakeFeaturesForArg sentinfo predidx (arg^.ma_argument) rngeach)]
  in (predidx,rolesetid,voicefeature,argfeatures)


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


features :: (SentenceInfo,[Instance]) -> [InstanceFeature]
features (sentinfo,prs) = 
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
      vmap = IM.fromList $ voice (pt,sentinfo^.corenlp_sent)
  in map (featuresForInstance sentinfo vmap) insts


fakeFeatures :: (SentenceInfo,[Instance]) -> [InstanceFeature]
fakeFeatures (sentinfo,prs) = 
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
      vmap = IM.fromList $ voice (pt,sentinfo^.corenlp_sent)
  in map (fakeFeaturesForInstance sentinfo vmap) insts


showFeatures :: (Int,SentenceInfo,[Instance]) -> IO ()
showFeatures (_i,sentinfo,prs) = do
  putStrLn "Truth items"
  putStrLn "---------------"
  mapM_ (putStrLn . formatInstanceFeature) (features (sentinfo,prs))
  putStrLn "---------------"
  

showFakeFeatures :: (Int,SentenceInfo,[Instance]) -> IO ()
showFakeFeatures (_i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
  putStrLn "Falsity items"
  putStrLn "---------------"
  mapM_ (putStrLn . formatInstanceFeature) (fakeFeatures (sentinfo,prs))
  putStrLn "---------------"
