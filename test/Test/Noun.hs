{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Noun where

import           Control.Lens                    ((^.),(%~),_1,_2,_3,_4,to)
import           Data.Bifunctor                  (bimap)
import qualified Data.IntMap             as IM
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Text.Format.Tree                (linePrint)
--
import           Data.Bitree                     (Bitree(..),getRoot1,toTree)
import           Data.BitreeZipper               (mkBitreeZipper)
import           NLP.Type.PennTreebankII         (PennTree,Lemma)
import           NLP.Type.TagPos                 (TagPos(..),TokIdx(..))
--
-- import           NLP.Syntax.Format               (formatBitree)
import           NLP.Syntax.Noun                 (bareNounModifier)
import           NLP.Syntax.Type                 (MarkType(..))
import           NLP.Syntax.Type.XBar            (Zipper)
import           NLP.Syntax.Util                 (mkBitreeICP)


type TestType = (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])

-- |
test_bare_noun_modifier_1 :: TestType
test_bare_noun_modifier_1 =
  ( "Billionaire environmentalist Tom Steyer"
  , [(0,("billionaire","Billionaire")),(1,("environmentalist","environmentalist")),(2,("Tom","Tom")),(3,("Steyer","Steyer"))]
  , PN "NP" [PL ("NN","Billionaire"),PL ("NN","environmentalist"),PL ("NNP","Tom"),PL ("NNP","Steyer")]
  , [TagPos (TokIdx 2,TokIdx 4,MarkEntity)]
  )


-- |
test_bare_noun_modifier_2 :: TestType
test_bare_noun_modifier_2 =
  ( "Uber Technologies Inc. co-founder Travis Kalanick"
  , [(0,("Uber","Uber")),(1,("Technologies","Technologies")),(2,("Inc.","Inc.")),(3,("co-founder","co-founder")),(4,("Travis","Travis")),(5,("Kalanick","Kalanick"))]
  , PN "NP" [PL ("NNP","Uber"),PL ("NNPS","Technologies"),PL ("NNP","Inc."),PL ("NN","co-founder"),PL ("NNP","Travis"),PL ("NNP","Kalanick")]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity),TagPos (TokIdx 4,TokIdx 6,MarkEntity)]
  )


testfunc :: TestType -> IO ()
testfunc x = do
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) (x^._2))
      lemmapt = mkBitreeICP lmap1 (x^._3)
      tr = toTree (bimap f g lemmapt)
        where f = (^._1.to show.to T.pack)
              g = (^._1.to show.to T.pack)
      z :: Zipper '[Lemma]
      z = getRoot1 $ mkBitreeZipper [] lemmapt
      -- y = bareNounModifier (x^._4) z
  -- print y
  T.IO.putStrLn (linePrint id tr)

