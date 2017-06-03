{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SRL.Feature where

import           Control.Lens            hiding (levels)
import           Control.Monad                  ((<=<),when)
import           Data.Bifunctor                 (bimap)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy,zip4)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text               as T   (intercalate,unpack)
import qualified Data.Text.IO            as TIO
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
import           SRL.PropBankMatch
import           SRL.Util
--
import Debug.Trace


data Position = Before | After | Embed
              deriving (Show,Eq,Ord)

data Direction = Up | Down
               deriving (Show,Eq,Ord)
                          
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


parseTreePathFull :: (Show c, Show p, Show a) =>
                     (Int,Range) -> PennTreeIdxG c (p,a)
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

annotateLevel :: IM.IntMap Int -> (Int, t) -> (Int,(Maybe Int,t))
annotateLevel levelmap (n,txt) = (n,(IM.lookup n levelmap,txt))

headWordTree :: Dependency -> Tree c (Int,t) -> Tree c (Int,(Maybe Int,t))
headWordTree (Dependency root nods edgs') tr =
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs'
      searchtree = head (dfs (buildG bnds edgs) [root])
      levelMap = IM.fromList  $ map (\(i,n) -> (i-1,n)) $ concat $ zipWith (\xs n -> map (,n) xs) (levels searchtree) [0..]
  in fmap (annotateLevel levelMap) tr 

headWord :: PennTreeIdxG ChunkTag (Maybe Int,(POSTag,Text)) -> Maybe Text
headWord  = safeHead . map snd . sortBy (compare `on` fst)
          . mapMaybe (\(_,(ml,(_,t))) -> (,) <$> ml <*> pure t) . getLeaves 

lemmatize :: IM.IntMap Text
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

showFeaturesForArgNode :: SentenceInfo -> Int -> Argument -> MatchedArgNode -> IO ()
showFeaturesForArgNode sentinfo predidx arg node = 
  when (arg ^. arg_label /= "rel")  $ do
    print (arg ^. arg_label)
    let rngs = node ^.. mn_trees . traverse . _1
    let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
        dep = sentinfo^.corenlp_dep
        parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
        opaths = map parseTreePath parsetrees
        paths = map (simplifyPTP . parseTreePath) parsetrees
        
        heads = map (\rng -> headWord =<< matchR rng (headWordTree dep ipt)) rngs
    if rngs == [(6,7)]
      then print (containR (6,7) ipt)
      else return ()
    mapM_ print (zip4 rngs opaths paths heads)

    
showFeaturesForArg :: SentenceInfo -> Int -> MatchedArgument -> IO ()
showFeaturesForArg sentinfo predidx arg = 
  mapM_ (showFeaturesForArgNode sentinfo predidx (arg^.ma_argument)) (arg^.ma_nodes)

  
showFeaturesForInstance :: SentenceInfo -> MatchedInstance -> IO ()
showFeaturesForInstance sentinfo inst = do
  print (inst ^. mi_instance.inst_lemma_type)
  let predidx = findRelNode (inst^.mi_arguments)
  mapM_ (showFeaturesForArg sentinfo predidx) (inst^.mi_arguments)


showVoice :: (PennTree,S.Sentence) -> IO ()
showVoice (pt,sent) = do
  TIO.putStrLn "---------- VOICE -----------------"
  let ipt = mkPennTreeIdx pt
      lemmamap =  foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> putStrLn (show n ++ ": " ++  T.unpack txt ++ ": " ++ show (isPassive z))
                  _ -> return ()
  mapM_ testf (mkTreeZipper [] lemmapt)



showFeatures :: (Int,SentenceInfo,[Instance]) -> IO ()
showFeatures (_i,sentinfo,prs) = do
  let pt = sentinfo^.corenlp_tree
      tr = sentinfo^.propbank_tree
      insts = matchInstances (pt,tr) prs
  mapM_ (showFeaturesForInstance sentinfo) insts
  showVoice (pt,sentinfo^.corenlp_sent)
