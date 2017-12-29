{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax where

import           Control.Lens                      ((^.))
import           Control.Monad.Trans.State         (evalState)
import qualified Data.IntMap                 as IM
--
import           NLP.Type.PennTreebankII           (Lemma)
--
import           NLP.Syntax.Clause                 (identifyCPHierarchy,bindingWH1,bindingWH2
                                                   ,mkX'TreePH1,resolveCP)
import           NLP.Syntax.Verb                   (verbPropertyFromPennTree)
import           NLP.Syntax.Type.PreAnalysis       (PreAnalysis(..),lemmaList,pennTree)
import           NLP.Syntax.Type.XBar              (X'Tree,Phase(..))


syntacticAnalysis :: PreAnalysis '[Lemma] -> [X'Tree 'PH1]
syntacticAnalysis pre =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) (pre^.lemmaList))
      vps  = verbPropertyFromPennTree lemmamap (pre^.pennTree)
      x'trs0 = (map resolveCP . identifyCPHierarchy pre) vps
  in map (bindingWH2 . (\tr -> evalState (bindingWH1 tr) 0) . mkX'TreePH1) x'trs0
