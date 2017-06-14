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
import           SRL.Feature.Dependency
import           SRL.Feature.ParseTreePath
import           SRL.Feature.Util
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--


calcSRLFeature :: SentenceInfo -> Int
               -> NodeRange
               -> Maybe SRLFeature -- Maybe (Range,ParseTreePath,Maybe (Int,(Level,(POSTag,Text))))
calcSRLFeature sentinfo predidx (Single rng) = 
  let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetree = parseTreePathFull (predidx,rng) ipt
      path = (simplifyPTP . parseTreePath) parsetree
      dltr = depLevelTree dep ipt
      hd = headWord =<< matchR rng dltr
  in  Just (SRLFeat rng path hd)
calcSRLFeature sentinfo predidx (Multi rngs) = 
  let ipt = mkPennTreeIdx (sentinfo^.corenlp_tree)
      dep = sentinfo^.corenlp_dep
      parsetrees = map (\rng -> parseTreePathFull (predidx,rng) ipt) rngs
      paths = map (simplifyPTP . parseTreePath) parsetrees
      dltr = depLevelTree dep ipt
      heads = map (\rng -> headWord =<< matchR rng dltr) rngs
      comparef Nothing  _        = GT
      comparef _        Nothing  = LT
      comparef (Just x) (Just y) = (compare `on` view (_2._1)) x y
  in  safeHead (sortBy (comparef `on` (view sfeat_headword)) $ zipWith3 SRLFeat rngs paths heads)

calcArgNodeFeature :: SentenceInfo -> Int -> ArgumentInput -> [ArgNodeFeature]
calcArgNodeFeature sentinfo predidx arginput =
  flip mapMaybe (arginput^.nodes) $ \n -> 
    AFeat (arginput^.pblabel) <$> calcSRLFeature sentinfo predidx n

  

calcInstanceFeature :: SentenceInfo -> InstanceInput -> InstanceFeature
calcInstanceFeature sentinfo input =
  let predidx = input^.predicate_id
      rolesetid = input^.lemma_roleset_id
      afeatss = map (calcArgNodeFeature sentinfo predidx) (input^.argument_inputs)
      voicemap = IM.fromList $ voice (sentinfo^.corenlp_tree,sentinfo^.corenlp_sent)      
      voicefeature = maybe Active snd (IM.lookup predidx voicemap)
  in IFeat predidx rolesetid voicefeature afeatss

     
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
