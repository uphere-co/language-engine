{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature where

import           Control.Lens            hiding (levels,Level)
import           Control.Monad                  ((<=<),guard)
import           Data.Bifunctor                 (bimap)
import           Data.Bifoldable                (biList)
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import           Data.Tree                      (levels)
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


mkLemmaMap :: S.Sentence -> IntMap Text
mkLemmaMap sent = foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))

lemmatize :: IntMap Text
          -> PennTreeIdxG ChunkTag (POSTag,Text)
          -> PennTreeIdxG ChunkTag (POSTag,(Text,Text))
lemmatize m = bimap id (\(i,(p,x)) -> (i,(p,(x,fromJust (IM.lookup i m)))))

findNotOverlappedNodes :: PennTreeIdx -> Range -> [Range]
findNotOverlappedNodes ipt rng = filter (`isNotOverlappedWith` rng)
                               . map (\(PN (r,_) _) -> r)
                               . filter (\case PN _ _ -> True ; _ -> False)
                               . biList
                               . duplicate 
                               $ ipt 
  

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


voice :: (PennTree,S.Sentence) -> [(Int,(Text,Voice))]
voice (pt,sent) = 
  let ipt = mkPennTreeIdx pt
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> Just (n,(txt,if isPassive z then Passive else Active))
                  _ -> Nothing
  in mapMaybe testf $ toList (mkTreeZipper [] lemmapt)


calcArgNodeFeatureEach :: SentenceInfo -> Int
                       -> NodeRange -- [Range]
                       -> Maybe (Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text))))
calcArgNodeFeatureEach sentinfo predidx (Single rng) = 
  let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetree = parseTreePathFull (predidx,rng) ipt
      -- opath = parseTreePath parsetree
      path = (simplifyPTP . parseTreePath) parsetree
      headwordtrees = headWordTree dep ipt
      hd = headWord =<< matchR rng headwordtrees
  in  Just (rng, path, hd)
calcArgNodeFeatureEach sentinfo predidx (Multi rngs) = 
  let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
      -- opaths = map parseTreePath parsetrees
      paths = map (simplifyPTP . parseTreePath) parsetrees
      headwordtrees = headWordTree dep ipt
      heads = map (\rng -> headWord =<< matchR rng headwordtrees) rngs
      comparef Nothing  _        = GT
      comparef _        Nothing  = LT
      comparef (Just x) (Just y) = (compare `on` view (_2._1)) x y
  in  safeHead (sortBy (comparef `on` (view _3)) $ zip3 rngs paths heads)

calcArgNodeFeature :: SentenceInfo -> Int -> ArgumentInput -> [ArgNodeFeature]
calcArgNodeFeature sentinfo predidx arginput =
  flip mapMaybe (arginput^.nodes) $ \n -> 
    (arginput^.pblabel,) <$> calcArgNodeFeatureEach sentinfo predidx n
    -- return (arginput^.pblabel,f)
  

calcInstanceFeature :: SentenceInfo -> InstanceInput -> InstanceFeature
calcInstanceFeature sentinfo input =
  let predidx = input^.predicate_id
      rolesetid = input^.lemma_roleset_id
      argfeatures = map (calcArgNodeFeature sentinfo predidx) (input^.argument_inputs)
      voicemap = IM.fromList $ voice (sentinfo^.corenlp_tree,sentinfo^.corenlp_sent)      
      voicefeature = maybe Active snd (IM.lookup predidx voicemap)
  in (predidx,rolesetid,voicefeature,argfeatures)

     
featuresForInstance :: SentenceInfo -> MatchedInstance -> InstanceFeature
featuresForInstance sentinfo inst = 
  let predidx = findRelNode (inst^.mi_arguments)
      rolesetid = inst^.mi_instance.inst_lemma_roleset_id
      arginputs = map argumentInputFromMatchedArgument
                . filter ((/= Relation) . (^.ma_argument.arg_label))
                $ inst^.mi_arguments
      input = InstanceInput predidx rolesetid arginputs
  in calcInstanceFeature sentinfo input 


fakeFeaturesForInstance :: SentenceInfo -> MatchedInstance -> InstanceFeature
fakeFeaturesForInstance sentinfo inst = 
  let predidx = findRelNode (inst^.mi_arguments)
      rolesetid = inst^.mi_instance.inst_lemma_roleset_id
      ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      args = filter ((/= Relation) . (^.ma_argument.arg_label)) (inst^.mi_arguments)
      mkRngs arg = do 
        let rngs = arg ^.. ma_nodes . traverse . mn_node . _1
        guard ((not.null) rngs)
        let rng = (minimum (map (^._1) rngs), maximum (map (^._2) rngs))
            exclst = findNotOverlappedNodes ipt rng 
        rngeach <- exclst
        guard (position predidx rngeach /= Embed)
        return (Single rngeach)
      arginputs = map (\arg -> ArgumentInput (arg^.ma_argument.arg_label) (mkRngs arg)) args 
      input = InstanceInput predidx rolesetid arginputs
  in calcInstanceFeature sentinfo input      
        

features :: (SentenceInfo,PennTree,[Instance]) -> [InstanceFeature]
features (sentinfo,propbanktree,prs) = 
  let insts = matchInstances (sentinfo^.corenlp_tree,propbanktree) prs
  in map (featuresForInstance sentinfo) insts


fakeFeatures :: (SentenceInfo,PennTree,[Instance]) -> [InstanceFeature]
fakeFeatures (sentinfo,propbanktree,prs) = 
  let insts = matchInstances (sentinfo^.corenlp_tree,propbanktree) prs
  in map (fakeFeaturesForInstance sentinfo) insts


showFeatures :: (Int,SentenceInfo,PennTree,[Instance]) -> IO ()
showFeatures (_i,sentinfo,propbanktree,prs) = do
  putStrLn "Truth items"
  putStrLn "---------------"
  mapM_ (putStrLn . formatInstanceFeature) (features (sentinfo,propbanktree,prs))
  putStrLn "---------------"
  

showFakeFeatures :: (Int,SentenceInfo,PennTree,[Instance]) -> IO ()
showFakeFeatures (_i,sentinfo,propbanktree,prs) = do
  putStrLn "Falsity items"
  putStrLn "---------------"
  mapM_ (putStrLn . formatInstanceFeature) (fakeFeatures (sentinfo,propbanktree,prs))
  putStrLn "---------------"
