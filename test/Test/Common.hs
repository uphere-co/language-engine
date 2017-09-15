{-# LANGUAGE DataKinds #-}

module Test.Common where

import qualified Data.IntMap             as IM
import           Data.Text                     (Text)
--
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Verb
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar



mkVPS :: [(Int,(Lemma,Text))] -> PennTree -> [VerbProperty (Zipper '[Lemma])]
mkVPS lmatknlst pt =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
  in verbPropertyFromPennTree lemmamap pt
