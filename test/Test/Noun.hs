{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Noun where

import           Control.Lens                    ((^.),(^..),(^?),(%~),_1,_2,_3,_4,_5,_Just,to)
import qualified Data.IntMap             as IM
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text               as T
--
import           Data.Bitree                     (Bitree(..),getRoot1)
import           Data.BitreeZipper               (mkBitreeZipper)
import           Data.Range                      (Range)
import           NLP.Type.NamedEntity            (NamedEntityClass(..))
import           NLP.Type.PennTreebankII         (PennTree,Lemma)
import           NLP.Type.TagPos                 (TagPos(..),TokIdx(..))
--
import           NLP.Syntax.Format.Internal      (formatDP)
import           NLP.Syntax.Noun                 (splitDP)
import           NLP.Syntax.Type                 (MarkType(..))
import           NLP.Syntax.Type.XBar            (TaggedLemma,DetP,AdjunctDP(..)
                                                 ,adjunct,complement,maximalProjection,specifier
                                                 ,headText,tokensByRange,mkOrdDP,compDPToRange)
import           NLP.Syntax.Util                 (mkBitreeICP,mkTaggedLemma)
--
import           Test.Common
import           Test.Tasty
import           Test.Tasty.HUnit



type TestNoun = (Text
                ,(Text,Maybe Text,Maybe Text,[Text])  -- ^ (head,spec,complement,adjunct)
                ,[(Int,(Lemma,Text))]
                ,PennTree,[TagPos TokIdx MarkType])

-- |
test_bare_noun_modifier_1 :: TestNoun
test_bare_noun_modifier_1 =
  ( "Billionaire environmentalist Tom Steyer"
  , ("Tom Steyer",Just "Billionaire environmentalist",Nothing,[])
  , [(0,("billionaire","Billionaire")),(1,("environmentalist","environmentalist")),(2,("Tom","Tom")),(3,("Steyer","Steyer"))]
  , PN "NP" [PL ("NN","Billionaire"),PL ("NN","environmentalist"),PL ("NNP","Tom"),PL ("NNP","Steyer")]
  , [TagPos (TokIdx 2,TokIdx 4,MarkEntity Person)]
  )


-- |
test_bare_noun_modifier_2 :: TestNoun
test_bare_noun_modifier_2 =
  ( "Uber Technologies Inc. co-founder Travis Kalanick"
  , ("Travis Kalanick",Just "Uber Technologies Inc. co-founder",Nothing,[])
  , [(0,("Uber","Uber")),(1,("Technologies","Technologies")),(2,("Inc.","Inc.")),(3,("co-founder","co-founder")),(4,("Travis","Travis")),(5,("Kalanick","Kalanick"))]
  , PN "NP" [PL ("NNP","Uber"),PL ("NNPS","Technologies"),PL ("NNP","Inc."),PL ("NN","co-founder"),PL ("NNP","Travis"),PL ("NNP","Kalanick")]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity Person),TagPos (TokIdx 4,TokIdx 6,MarkEntity Person)]
  )

-- |
test_bare_noun_modifier_3 :: TestNoun
test_bare_noun_modifier_3 =
  ( "Mexican state oil company Pemex"
  , ("Pemex",Just "Mexican state oil company",Nothing,[])
  , [(0,("mexican","Mexican")),(1,("state","state")),(2,("oil","oil")),(3,("company","company")),(4,("Pemex","Pemex"))]
  , PN "NP" [PN "NP" [PL ("JJ","Mexican"),PL ("NN","state"),PL ("NN","oil"),PL ("NN","company")],PN "NP" [PL ("NNP","Pemex")]]
  , [TagPos (TokIdx 4,TokIdx 5,MarkEntity Org)]
  )


-- |
test_bare_noun_modifier_4 :: TestNoun
test_bare_noun_modifier_4 =
  ( "its food delivery service"
  , ("its food delivery service",Nothing,Nothing,[])
  , [(0,("its","its")),(1,("food","food")),(2,("delivery","delivery")),(3,("service","service"))]
  , PN "NP" [PN "NP" [PL ("PRP$","its"),PL ("NN","food"),PL ("NN","delivery")],PN "NP" [PL ("NN","service")]]
  , [TagPos (TokIdx 3, TokIdx 4,MarkEntity Other)]
  )


-- |
test_paren_modifier_1 :: TestNoun
test_paren_modifier_1 =
  ( "Los-Angeles-based company, Hyperloop One"
  , ("Hyperloop One",Just "Los-Angeles-based company",Nothing,[])
  , [(0,("los-angeles-based","Los-Angeles-based")),(1,("company","company")),(2,(",",",")),(3,("Hyperloop","Hyperloop")),(4,("one","One"))]
  , PN "NP" [PN "NP" [PL ("JJ","Los-Angeles-based"),PL ("NN","company")],PL (",",","),PN "NP" [PL ("NNP","Hyperloop"),PL ("CD","One")]]
  , [TagPos (TokIdx 3, TokIdx 5, MarkEntity Org)]
  )

-- |
test_prep_modifier_1 :: TestNoun
test_prep_modifier_1 =
  ( "an initial public offering on the country's stock exchange"
  , ("an initial public offering",Nothing,Nothing,["on the country 's stock exchange"])
  , [(0,("a","an")),(1,("initial","initial")),(2,("public","public")),(3,("offering","offering")),(4,("on","on")),(5,("the","the")),(6,("country","country")),(7,("'s","'s")),(8,("stock","stock")),(9,("exchange","exchange"))]
  , PN "NP" [PN "NP" [PL ("DT","an"),PL ("JJ","initial"),PL ("JJ","public"),PL ("NN","offering")],PN "PP" [PL ("IN","on"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","country"),PL ("POS","'s")],PL ("NN","stock"),PL ("NN","exchange")]]]
  , []
  )

-- |
test_prep_modifier_2 :: TestNoun
test_prep_modifier_2 =
  ( "his criticism of silent player"
  , ("his criticism",Nothing, Just "of silent player",[])
  , [(0,("he","his")),(1,("criticism","criticism")),(2,("of","of")),(3,("silent","silent")),(4,("player","player"))]
  , PN "ROOT" [PN "NP" [PN "NP" [PL ("PRP$","his"),PL ("NN","criticism")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","silent"),PL ("NN","player")]]]]
  , []
  )

test_prep_modifier_3 :: TestNoun
test_prep_modifier_3 =
  ( "Hain Celestial Group Inc, under pressure from activist investor Engaged Capital LLC,"
  , ("Hain Celestial Group Inc",Nothing,Nothing,["under pressure from activist investor Engaged Capital LLC"])
  , [(0,("Hain","Hain")),(1,("Celestial","Celestial")),(2,("Group","Group")),(3,("Inc","Inc")),(4,(",",",")),(5,("under","under")),(6,("pressure","pressure")),(7,("from","from")),(8,("activist","activist")),(9,("investor","investor")),(10,("Engaged","Engaged")),(11,("Capital","Capital")),(12,("LLC","LLC")),(13,(",",","))]
  , PN "NP" [PN "NP" [PL ("NNP","Hain"),PL ("NNP","Celestial"),PL ("NNP","Group"),PL ("NNP","Inc")],PL (",",","),PN "PP" [PL ("IN","under"),PN "NP" [PN "NP" [PL ("NN","pressure")],PN "PP" [PL ("IN","from"),PN "NP" [PL ("JJ","activist"),PL ("NN","investor"),PL ("NNP","Engaged"),PL ("NNP","Capital"),PL ("NNP","LLC")]]]],PL (",",",")]
  , [TagPos (TokIdx 0,TokIdx 4,MarkEntity Org), TagPos (TokIdx 10,TokIdx 13,MarkEntity Org)]
  )

test_appos_or_1 :: TestNoun
test_appos_or_1 =
  ("$154.5 million, or 54 cents a share,"
  , ("$ 154.5 million",Just "54 cents a share",Nothing,[])
  , [(0,("$","$")),(1,("154.5","154.5")),(2,("million","million")),(3,(",",",")),(4,("or","or")),(5,("54","54")),(6,("cent","cents")),(7,("a","a")),(8,("share","share")),(9,(",",","))]
  , PN "NP" [PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","154.5"),PL ("CD","million")]],PL (",",","),PL ("CC","or"),PN "NP" [PN "NP" [PL ("CD","54"),PL ("NNS","cents")],PN "NP" [PL ("DT","a"),PL ("NN","share")]],PL (",",",")]
  , []
  )


mkDPFromTest :: TaggedLemma '[Lemma] -> TestNoun -> DetP '[Lemma]
mkDPFromTest tagged x =
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) (x^._3))
      lemmapt = mkBitreeICP lmap1 (x^._4)
      z = getRoot1 $ mkBitreeZipper [] lemmapt
  in splitDP tagged (mkOrdDP z)



adjunctDPToRange :: AdjunctDP t -> Range
adjunctDPToRange (AdjunctDP_Unresolved rng) = rng
adjunctDPToRange (AdjunctDP_PP pp) = pp^.maximalProjection


checkBNM :: TestNoun -> Bool
checkBNM x =
  let tagged = mkTaggedLemma (x^._3) (x^._4) (x^._5)
      dp = mkDPFromTest tagged x
  in (headText tagged dp == x^._2._1)
     &&
     ((dp^?specifier._Just.to (T.intercalate " " . tokensByRange tagged)) == (x^._2._2))
     &&
     ((dp^?complement.complement._Just.to (T.intercalate " " . tokensByRange tagged . compDPToRange)) == (x^._2._3))
     &&
     ((dp^..adjunct.traverse.to (T.intercalate " " . tokensByRange tagged . adjunctDPToRange)) == (x^._2._4))


testcases :: [TestNoun]
testcases = [ test_bare_noun_modifier_1
            , test_bare_noun_modifier_2
            , test_bare_noun_modifier_3
            , test_bare_noun_modifier_4
            , test_paren_modifier_1
            , test_prep_modifier_1
            , test_prep_modifier_2
            , test_prep_modifier_3
            , test_appos_or_1
            ]


unitTests :: TestTree
unitTests = testGroup "Bare Noun Modifier test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkBNM c @? (let (txt,_,lma,pt,taglst) = c
                                   tagged = mkTaggedLemma lma pt taglst
                               in T.unpack $ T.intercalate "\n" (formatDetail (txt,lma,pt,taglst)) <>
                                             "\n" <>
                                             formatDP (mkDPFromTest tagged c)
                              )




{-

test_internal_time ::  (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_internal_time
  = ( "Toyota Motor Corp on Monday"
    , [(0,("Toyota","Toyota")),(1,("Motor","Motor")),(2,("Corp","Corp")),(3,("on","on")),(4,("Monday","Monday"))]
    , PN "NP" [PN "NP" [PL ("NNP","Toyota"),PL ("NNP","Motor"),PL ("NNP","Corp")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Monday")]]]
    , [TagPos (TokIdx 3, TokIdx 4, MarkTime)]
    )

testFunc t =
  let lmas = t^._2
      tr = t^._3
      tagged = t^._4
      lmap1 = IM.fromList (map (_2 %~ (^._1)) (t^._2))
      lemmapt = mkBitreeICP lmap1 tr
      z = getRoot1 $ mkBitreeZipper [] lemmapt
      dp = mkOrdDP z
      (dp',zs) = identifyInternalTimePrep tagged dp
  in T.intercalate "\n" (map (getTokens.current) zs) <> "\n" <>
     formatDP dp' <> "\n" <>
     T.pack (show (dp'^.headX))
-}
