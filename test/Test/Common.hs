{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Common where

import           Control.Lens
import           Data.Foldable                   (toList)
import qualified Data.IntMap             as IM
import           Data.List                       (find)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Printer.PennTreebankII      (prettyPrint)
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.TagPos
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Preposition
import           NLP.Syntax.Verb
import           NLP.Syntax.Type                 (MarkType(..))
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util



mkVPS :: [(Int,(Lemma,Text))] -> PennTree -> [VerbProperty (Zipper '[Lemma])]
mkVPS lmatknlst pt =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
  in verbPropertyFromPennTree lemmamap pt


formatDetail :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType]) -> [Text]
formatDetail (_txt,lma,pt,tagged) =
  let vps  = mkVPS lma pt
      clausetr = clauseStructure tagged vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx pt))
      x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps

  in
  [ "===================================================================================================================="
  , (T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList) pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]
  ++ map formatX'Tree x'tr
  ++ map (formatVPwithPAWS tagged clausetr x'tr) vps
  ++
  [ "--------------------------------------------------------------------------------------------------------------------"
  , prettyPrint 0 pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]



showDetail0 :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType]) -> IO ()
showDetail0 (txt,lma,pt,tmxs) = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn txt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList $ pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) lma)
  showClauseStructure tmxs lmap1 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn $ prettyPrint 0 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  flip mapM_ tmxs $ \(TagPos (b,e,_tag)) -> do
    let lemmapt = mkBitreeICP lmap1 pt
        rng = beginEndToRange (b,e)
    case find (\z -> getRoot (current z) ^? _Left . _1  == Just rng) $ getNodes (mkBitreeZipper [] lemmapt) of
      Nothing -> return ()
      Just z -> print $ hasEmptyPreposition z
