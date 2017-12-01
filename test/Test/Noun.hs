{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Noun where

import           Control.Lens                    ((^.),(^..),(^?),(%~),_1,_2,_3,_4,_5,_6,_Just,to)
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
import           WordNet.Type.Lexicographer      (LexicographerFile)
--
import           NLP.Syntax.Format.Internal      (formatDP)
import           NLP.Syntax.Noun                 (splitDP)
import           NLP.Syntax.Type                 (MarkType(..))
import           NLP.Syntax.Type.XBar            (TaggedLemma,DetP,AdjunctDP(..)
                                                 ,adjunct,complement,headX,maximalProjection,specifier
                                                 ,hd_range,headText,tokensByRange,mkOrdDP,compDPToRange
                                                 ,specDPText
                                                 )
import           NLP.Syntax.Util                 (mkBitreeICP,mkTaggedLemma)
--
import           Test.Common
import           Test.Tasty
import           Test.Tasty.HUnit

-- import Debug.Trace


type TestNoun = (Text
                ,(Text,Maybe Text,[Text],Maybe Text,[Text])  -- ^ (determiner,headnoun,spec,complement,adjunct)
                ,[(Int,(Lemma,Text))]
                ,PennTree
                ,[TagPos TokIdx MarkType]
                ,[(Int,LexicographerFile)]
                )

-- |
test_bare_noun_modifier_1 :: TestNoun
test_bare_noun_modifier_1 =
  ( "Billionaire environmentalist Tom Steyer"
  , ("",Just "Tom Steyer",["Billionaire environmentalist"],Nothing,[])
  , [(0,("billionaire","Billionaire")),(1,("environmentalist","environmentalist")),(2,("Tom","Tom")),(3,("Steyer","Steyer"))]
  , PN "NP" [PL ("NN","Billionaire"),PL ("NN","environmentalist"),PL ("NNP","Tom"),PL ("NNP","Steyer")]
  , [TagPos (TokIdx 2,TokIdx 4,MarkEntity Person)]
  , []
  )


-- |
test_bare_noun_modifier_2 :: TestNoun
test_bare_noun_modifier_2 =
  ( "Uber Technologies Inc. co-founder Travis Kalanick"
  , ("",Just "Travis Kalanick",["Uber Technologies Inc. co-founder"],Nothing,[])
  , [(0,("Uber","Uber")),(1,("Technologies","Technologies")),(2,("Inc.","Inc.")),(3,("co-founder","co-founder")),(4,("Travis","Travis")),(5,("Kalanick","Kalanick"))]
  , PN "NP" [PL ("NNP","Uber"),PL ("NNPS","Technologies"),PL ("NNP","Inc."),PL ("NN","co-founder"),PL ("NNP","Travis"),PL ("NNP","Kalanick")]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity Person),TagPos (TokIdx 4,TokIdx 6,MarkEntity Person)]
  , []
  )

-- |
test_bare_noun_modifier_3 :: TestNoun
test_bare_noun_modifier_3 =
  ( "Mexican state oil company Pemex"
  , ("",Just "Pemex",["Mexican state oil company"],Nothing,[])
  , [(0,("mexican","Mexican")),(1,("state","state")),(2,("oil","oil")),(3,("company","company")),(4,("Pemex","Pemex"))]
  , PN "NP" [PN "NP" [PL ("JJ","Mexican"),PL ("NN","state"),PL ("NN","oil"),PL ("NN","company")],PN "NP" [PL ("NNP","Pemex")]]
  , [TagPos (TokIdx 4,TokIdx 5,MarkEntity Org)]
  , []
  )


-- | this is problematic
test_bare_noun_modifier_4 :: TestNoun
test_bare_noun_modifier_4 =
  ( "its food delivery service"
  , ("its",Just "food delivery service",[],Nothing,[])
  , [(0,("its","its")),(1,("food","food")),(2,("delivery","delivery")),(3,("service","service"))]
  , PN "NP" [PN "NP" [PL ("PRP$","its"),PL ("NN","food"),PL ("NN","delivery")],PN "NP" [PL ("NN","service")]]
  , [TagPos (TokIdx 3, TokIdx 4,MarkEntity Other)]
  , []
  )


-- |
test_paren_modifier_1 :: TestNoun
test_paren_modifier_1 =
  ( "Los-Angeles-based company, Hyperloop One"
  , ("",Just "Hyperloop One",["Los-Angeles-based company"],Nothing,[])
  , [(0,("los-angeles-based","Los-Angeles-based")),(1,("company","company")),(2,(",",",")),(3,("Hyperloop","Hyperloop")),(4,("one","One"))]
  , PN "NP" [PN "NP" [PL ("JJ","Los-Angeles-based"),PL ("NN","company")],PL (",",","),PN "NP" [PL ("NNP","Hyperloop"),PL ("CD","One")]]
  , [TagPos (TokIdx 3, TokIdx 5, MarkEntity Org)]
  , []
  )


-- |
test_paren_modifier_2 :: TestNoun
test_paren_modifier_2 =
  ( "Germany's largest independent pizza chain, Hallo Pizza,"
  , ("",Just "Hallo Pizza",["Germany 's largest independent pizza chain"],Nothing,[])

  , [(0,("Germany","Germany")),(1,("'s","'s")),(2,("largest","largest")),(3,("independent","independent")),(4,("pizza","pizza")),(5,("chain","chain")),(6,(",",",")),(7,("Hallo","Hallo")),(8,("Pizza","Pizza")),(9,(",",","))]
  , PN "NP" [PN "NP" [PN "NP" [PL ("NNP","Germany"),PL ("POS","'s")],PN "ADJP" [PL ("JJS","largest"),PL ("JJ","independent")],PL ("NN","pizza"),PL ("NN","chain")],PL (",",","),PN "NP" [PL ("NNP","Hallo"),PL ("NNP","Pizza")],PL (",",",")]
  , [TagPos (TokIdx 7, TokIdx 9, MarkEntity Org)]
  , []
  )

-- |
test_paren_modifier_3 :: TestNoun
test_paren_modifier_3 =
  ( "SF Motors Inc, a California-based electric vehicle unit of China's Chongqing Sokon Industry Group Co Ltd,"
  , ("",Just "SF Motors Inc",["a California-based electric vehicle unit of China 's Chongqing Sokon Industry Group Co Ltd"],Nothing,[])
  , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,(",",",")),(4,("a","a")),(5,("california-based","California-based")),(6,("electric","electric")),(7,("vehicle","vehicle")),(8,("unit","unit")),(9,("of","of")),(10,("China","China")),(11,("'s","'s")),(12,("Chongqing","Chongqing")),(13,("Sokon","Sokon")),(14,("Industry","Industry")),(15,("Group","Group")),(16,("Co","Co")),(17,("Ltd","Ltd")),(18,(",",","))]
  , PN "NP" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","California-based"),PL ("JJ","electric"),PL ("NN","vehicle"),PL ("NN","unit")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNP","Chongqing"),PL ("NNP","Sokon"),PL ("NNP","Industry"),PL ("NNP","Group"),PL ("NNP","Co"),PL ("NNP","Ltd")]]],PL (",",",")]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity Org)]
  , []
  )


-- |
test_prep_modifier_1 :: TestNoun
test_prep_modifier_1 =
  ( "an initial public offering on the country's stock exchange"
  , ("an",Just "initial public offering",[],Nothing,["on the country 's stock exchange"])
  , [(0,("a","an")),(1,("initial","initial")),(2,("public","public")),(3,("offering","offering")),(4,("on","on")),(5,("the","the")),(6,("country","country")),(7,("'s","'s")),(8,("stock","stock")),(9,("exchange","exchange"))]
  , PN "NP" [PN "NP" [PL ("DT","an"),PL ("JJ","initial"),PL ("JJ","public"),PL ("NN","offering")],PN "PP" [PL ("IN","on"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","country"),PL ("POS","'s")],PL ("NN","stock"),PL ("NN","exchange")]]]
  , []
  , []
  )

-- |
test_prep_modifier_2 :: TestNoun
test_prep_modifier_2 =
  ( "his criticism of silent player"
  , ("his",Just "criticism",[], Just "of silent player",[])
  , [(0,("he","his")),(1,("criticism","criticism")),(2,("of","of")),(3,("silent","silent")),(4,("player","player"))]
  , PN "ROOT" [PN "NP" [PN "NP" [PL ("PRP$","his"),PL ("NN","criticism")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","silent"),PL ("NN","player")]]]]
  , []
  , []
  )

test_prep_modifier_3 :: TestNoun
test_prep_modifier_3 =
  ( "Hain Celestial Group Inc, under pressure from activist investor Engaged Capital LLC,"
  , ("",Just "Hain Celestial Group Inc",[],Nothing,["under pressure from activist investor Engaged Capital LLC"])
  , [(0,("Hain","Hain")),(1,("Celestial","Celestial")),(2,("Group","Group")),(3,("Inc","Inc")),(4,(",",",")),(5,("under","under")),(6,("pressure","pressure")),(7,("from","from")),(8,("activist","activist")),(9,("investor","investor")),(10,("Engaged","Engaged")),(11,("Capital","Capital")),(12,("LLC","LLC")),(13,(",",","))]
  , PN "NP" [PN "NP" [PL ("NNP","Hain"),PL ("NNP","Celestial"),PL ("NNP","Group"),PL ("NNP","Inc")],PL (",",","),PN "PP" [PL ("IN","under"),PN "NP" [PN "NP" [PL ("NN","pressure")],PN "PP" [PL ("IN","from"),PN "NP" [PL ("JJ","activist"),PL ("NN","investor"),PL ("NNP","Engaged"),PL ("NNP","Capital"),PL ("NNP","LLC")]]]],PL (",",",")]
  , [TagPos (TokIdx 0,TokIdx 4,MarkEntity Org), TagPos (TokIdx 10,TokIdx 13,MarkEntity Org)]
  , []
  )

test_prep_modifier_4 :: TestNoun
test_prep_modifier_4 =
  ( "General Motors Co's vehicle sales in China"
  , ("'s",Just "vehicle sales",["General Motors Co"],Nothing,["in China"])
  , [(0,("General","General")),(1,("Motors","Motors")),(2,("Co","Co")),(3,("'s","'s")),(4,("vehicle","vehicle")),(5,("sale","sales")),(6,("in","in")),(7,("China","China"))]
  , PN "NP" [PN "NP" [PN "NP" [PL ("NNP","General"),PL ("NNPS","Motors"),PL ("NNP","Co"),PL ("POS","'s")],PL ("NN","vehicle"),PL ("NNS","sales")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("NNP","China")]]]
  , [TagPos (TokIdx 0,TokIdx 3,MarkEntity Org)]
  , []
  )



test_appos_or_1 :: TestNoun
test_appos_or_1 =
  ("$154.5 million, or 54 cents a share,"
  , ("",Just "$ 154.5 million",["54 cents a share"],Nothing,[])
  , [(0,("$","$")),(1,("154.5","154.5")),(2,("million","million")),(3,(",",",")),(4,("or","or")),(5,("54","54")),(6,("cent","cents")),(7,("a","a")),(8,("share","share")),(9,(",",","))]
  , PN "NP" [PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","154.5"),PL ("CD","million")]],PL (",",","),PL ("CC","or"),PN "NP" [PN "NP" [PL ("CD","54"),PL ("NNS","cents")],PN "NP" [PL ("DT","a"),PL ("NN","share")]],PL (",",",")]
  , []
  , []
  )

test_article_1 :: TestNoun
test_article_1 =
  ( "the proposed $25 billion merger between fertiliser companies Agrium Inc and Potash Corp of Saskatchewan Inc"
  , ("the",Just "proposed $ 25 billion merger", [],Nothing,["between fertiliser companies Agrium Inc and Potash Corp of Saskatchewan Inc"])
  , [(0,("the","the")),(1,("propose","proposed")),(2,("$","$")),(3,("25","25")),(4,("billion","billion")),(5,("merger","merger")),(6,("between","between")),(7,("fertiliser","fertiliser")),(8,("company","companies")),(9,("Agrium","Agrium")),(10,("Inc","Inc")),(11,("and","and")),(12,("Potash","Potash")),(13,("Corp","Corp")),(14,("of","of")),(15,("Saskatchewan","Saskatchewan")),(16,("Inc","Inc"))]
  , PN "NP" [PN "NP" [PL ("DT","the"),PL ("VBN","proposed"),PN "ADJP" [PN "QP" [PL ("$","$"),PL ("CD","25"),PL ("CD","billion")]],PL ("NN","merger")],PN "PP" [PL ("IN","between"),PN "NP" [PN "NP" [PL ("NN","fertiliser"),PL ("NNS","companies"),PL ("NNP","Agrium"),PL ("NNP","Inc"),PL ("CC","and"),PL ("NNP","Potash"),PL ("NNP","Corp")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNP","Saskatchewan"),PL ("NNP","Inc")]]]]]
  , []
  , []
  )


test_possesive_clitic_1 :: TestNoun
test_possesive_clitic_1 =
  ( "Takeda Pharmaceutical 's experimental dengue vaccine"
  , ("'s",Just "experimental dengue vaccine",["Takeda Pharmaceutical"],Nothing,[])
  , [(0,("Takeda","Takeda")),(1,("Pharmaceutical","Pharmaceutical")),(2,("'s","'s")),(3,("experimental","experimental")),(4,("dengue","dengue")),(5,("vaccine","vaccine"))]
  , PN "NP" [PN "NP" [PL ("NNP","Takeda"),PL ("NNP","Pharmaceutical"),PL ("POS","'s")],PL ("JJ","experimental"),PL ("NN","dengue"),PL ("NN","vaccine")]
  , [TagPos (TokIdx 0,TokIdx 2,MarkEntity Org)]
  , []
  )

test_infinitive_modifier_1 :: TestNoun
test_infinitive_modifier_1 =
  ( "its plan to seek approval of an environmental impact study"
  , ("its",Just "plan",[],Just "to seek approval of an environmental impact study",[])
  , [(0,("its","its")),(1,("plan","plan")),(2,("to","to")),(3,("seek","seek")),(4,("approval","approval")),(5,("of","of")),(6,("a","an")),(7,("environmental","environmental")),(8,("impact","impact")),(9,("study","study"))]
  , PN "NP" [PL ("PRP$","its"),PL ("NN","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","seek"),PN "NP" [PN "NP" [PL ("NN","approval")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","an"),PL ("JJ","environmental"),PL ("NN","impact"),PL ("NN","study")]]]]]]]
  , []
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
  let tagged = mkTaggedLemma (x^._3) (x^._4) (x^._5) (x^._6)
      dp = mkDPFromTest tagged x
      result = (dp^.headX.hd_range.to (maybe "" (T.intercalate " " . tokensByRange tagged))
               ,fmap (headText tagged) (dp^.complement)
               ,dp^..specifier.traverse.to (specDPText tagged)
               ,dp^?complement._Just.complement._Just.to (T.intercalate " " . tokensByRange tagged . compDPToRange)
               ,dp^..adjunct.traverse.to (T.intercalate " " . tokensByRange tagged . adjunctDPToRange)
               )
  in result == x^._2


testcases :: [TestNoun]
testcases = [ test_bare_noun_modifier_1
            , test_bare_noun_modifier_2
            , test_bare_noun_modifier_3
            , test_bare_noun_modifier_4
            , test_paren_modifier_1
            , test_paren_modifier_2
            , test_paren_modifier_3
            , test_prep_modifier_1
            , test_prep_modifier_2
            , test_prep_modifier_3
            , test_prep_modifier_4
            , test_appos_or_1
            , test_article_1
            , test_possesive_clitic_1
            , test_infinitive_modifier_1
            ]


unitTests :: TestTree
unitTests = testGroup "Bare Noun Modifier test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkBNM c @? (let (txt,_,lma,pt,taglst,synsets) = c
                                   tagged = mkTaggedLemma lma pt taglst synsets
                               in T.unpack $ T.intercalate "\n" (formatDetail (txt,lma,pt,taglst,synsets)) <>
                                             "\n" <>
                                             formatDP (mkDPFromTest tagged c)
                              )
