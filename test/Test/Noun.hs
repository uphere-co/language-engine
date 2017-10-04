{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Noun where

import           Control.Lens                    ((^.),(%~),_1,_2,_3,_4,_5,to)
import           Data.Bifunctor                  (bimap)
import qualified Data.IntMap             as IM
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Text.Format.Tree                (linePrint)
--
import           Data.Bitree                     (Bitree(..),getRoot1,toTree)
import           Data.BitreeZipper               (mkBitreeZipper)
import           Data.Range                      (Range)
import           NLP.Type.PennTreebankII         (PennTree,Lemma)
import           NLP.Type.TagPos                 (TagPos(..),TokIdx(..))
--
import           NLP.Syntax.Noun                 (bareNounModifier,mkOrdDP)
import           NLP.Syntax.Type                 (MarkType(..))
import           NLP.Syntax.Type.XBar            (Zipper,headX)
import           NLP.Syntax.Util                 (mkBitreeICP)
--
import           Test.Tasty
import           Test.Tasty.HUnit

type TestBNM = (Text,(Range,Range),[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])

-- |
test_bare_noun_modifier_1 :: TestBNM
test_bare_noun_modifier_1 =
  ( "Billionaire environmentalist Tom Steyer"
  , ((0,3),(2,3))
  , [(0,("billionaire","Billionaire")),(1,("environmentalist","environmentalist")),(2,("Tom","Tom")),(3,("Steyer","Steyer"))]
  , PN "NP" [PL ("NN","Billionaire"),PL ("NN","environmentalist"),PL ("NNP","Tom"),PL ("NNP","Steyer")]
  , [TagPos (TokIdx 2,TokIdx 4,MarkEntity)]
  )


-- |
test_bare_noun_modifier_2 :: TestBNM
test_bare_noun_modifier_2 =
  ( "Uber Technologies Inc. co-founder Travis Kalanick"
  , ((0,5),(4,5))
  , [(0,("Uber","Uber")),(1,("Technologies","Technologies")),(2,("Inc.","Inc.")),(3,("co-founder","co-founder")),(4,("Travis","Travis")),(5,("Kalanick","Kalanick"))]
  , PN "NP" [PL ("NNP","Uber"),PL ("NNPS","Technologies"),PL ("NNP","Inc."),PL ("NN","co-founder"),PL ("NNP","Travis"),PL ("NNP","Kalanick")]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity),TagPos (TokIdx 4,TokIdx 6,MarkEntity)]
  )

-- |
test_bare_noun_modifier_3 :: TestBNM
test_bare_noun_modifier_3 =
  ( "Mexican state oil company Pemex"
  , ((0,4),(4,4))
  , [(0,("mexican","Mexican")),(1,("state","state")),(2,("oil","oil")),(3,("company","company")),(4,("Pemex","Pemex"))]
  , PN "NP" [PN "NP" [PL ("JJ","Mexican"),PL ("NN","state"),PL ("NN","oil"),PL ("NN","company")],PN "NP" [PL ("NNP","Pemex")]]
  , [TagPos (TokIdx 4,TokIdx 5,MarkEntity)]
  )


checkBNM :: TestBNM -> Bool
checkBNM x =
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) (x^._3))
      lemmapt = mkBitreeICP lmap1 (x^._4)
      tr = toTree (bimap f g lemmapt)
        where f = (^._1.to show.to T.pack)
              g = (^._1.to show.to T.pack)
      z :: Zipper '[Lemma]
      z = getRoot1 $ mkBitreeZipper [] lemmapt
      y = bareNounModifier (x^._5) (mkOrdDP z)
  in (y^.headX == x^._2)

--  T.IO.putStrLn (linePrint id tr)


testcases :: [TestBNM]
testcases = [ test_bare_noun_modifier_1
            , test_bare_noun_modifier_2
            , test_bare_noun_modifier_3
            ]


unitTests :: TestTree
unitTests = testGroup "Bare Noun Modifier test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkBNM c @? "error"


