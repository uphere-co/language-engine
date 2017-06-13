{-# LANGUAGE TupleSections #-}

module SRL.Feature.ParseTreePath where

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
import           SRL.Feature.Util
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--


parseTreePathFull :: (Int,Range)
                  -> PennTreeIdxG c (p,a)
                  -> (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
parseTreePathFull (start,target) tr = elimCommonHead (contain start tr) (containR target tr)


parseTreePathBy :: (PennTreeIdxG c (p,a) -> x)
                -> (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
                -> [(x,Direction)]
parseTreePathBy f (mh,tostart,totarget) =
  case mh of
    Nothing -> []
    Just h -> let lst1 = (f h,Down):map ((,Down).f) totarget
                  lst2 = map ((,Up).f) . reverse $ tostart
              in lst2 ++ lst1


parseTreePath :: (Maybe (PennTreeIdxG c (p,a)),[PennTreeIdxG c (p,a)],[PennTreeIdxG c (p,a)])
              -> [(Either c p,Direction)]
parseTreePath = parseTreePathBy (snd.phraseType)


simplifyPTP :: (Eq c,Eq p) => [(Either c p, Direction)] -> [(Either c p, Direction)]
simplifyPTP xs = map head (group xs)


simplifyDep :: (DepInfo,[DepInfo],[DepInfo]) -> (DepInfo,[DepInfo],[DepInfo])
simplifyDep (y,xs,zs) = (y,map head (tail (group (y:xs))),map head (tail (group (y:zs))))

