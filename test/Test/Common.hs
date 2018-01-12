{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common where

import           Data.Foldable                   (toList)
import qualified Data.IntMap             as IM
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text               as T
--
import           NLP.Printer.PennTreebankII      (prettyPrint)
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos
import           WordNet.Type.Lexicographer      (LexicographerFile)
--
import           NLP.Syntax                      (syntacticAnalysis)
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Verb
import           NLP.Syntax.Type.Resolve         (retrieveResolved)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util



mkVPS :: [(Int,(Lemma,Text))] -> PennTree -> [VerbProperty (Zipper '[Lemma])]
mkVPS lmatknlst pt =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
  in verbPropertyFromPennTree lemmamap pt


formatX'TreeAndResolution :: X'Tree 'PH1 -> Text
formatX'TreeAndResolution x'tr = formatX'Tree SPH1 x'tr <> "\n" <> T.pack (show (retrieveResolved x'tr))



formatDetail :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType],[(Int,LexicographerFile)]) -> [Text]
formatDetail (_txt,lma,pt,taglst,synsets) =
  let pre = mkPreAnalysis lma pt taglst synsets
      x'trs = syntacticAnalysis pre

      vps  = mkVPS lma pt
      x'trs0 = (map resolveCP . identifyCPHierarchy pre) vps
  in
  [ "===================================================================================================================="
  , (T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList) pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]
  ++ ["PHASE0"]
  ++ map (formatX'Tree SPH0) x'trs0
  ++ ["PHASE1"]
  ++ map formatX'TreeAndResolution x'trs
  ++
  [ "--------------------------------------------------------------------------------------------------------------------"
  , prettyPrint 0 pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]


