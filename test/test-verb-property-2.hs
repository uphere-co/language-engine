{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels)
import           Control.Monad                     ((<=<))
import qualified Data.IntMap                as IM
import           Data.List                         (find)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           CoreNLP.Simple.Type.Simplified
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Format
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
--


ex2 = ( "He will be fined $25,000."
      , 3
      , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]       
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      )


ex4 = ( "The move had been widely expected."
      , 5
      , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
      )


-- | Reduced relative clause.
ex25 = ( "NASA enhances online scientific tool used by hundreds of scientists."
       , 5
       , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
       )


mkVPS :: [(Int,(Lemma,Text))] -> PennTree -> [VerbProperty (BitreeZipperICP '[Lemma])]
mkVPS lmatknlst pt =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
  in verbPropertyFromPennTree lemmamap pt




showVP :: (Text,Int,[(Int,(Lemma,Text))],PennTree) -> IO ()
showVP (txt,i,lmatknlst,pt) = do
  let vps = mkVPS lmatknlst pt
  case find (\vp -> vp^.vp_index == i) vps of
    Nothing -> error "nothing"
    Just vp -> do
      T.IO.putStrLn (prettyPrint 0 pt)
      case (vp^.vp_words) of
        [] -> error "nothing2"
        z:_ ->
          let tag = (bimap (chunkTag.snd) (posTag.snd) . getRoot . current) <$>
                      ((parent <=< parent) (fst z))
          in print tag



main :: IO ()
main = do
  showVP ex2
  showVP ex4
  showVP ex25
