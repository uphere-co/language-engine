{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import qualified Data.IntMap                as IM
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.ListZipper
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.TagPos
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                   (mkBitreeICP,mkTaggedLemma)
--
import           Test.Common
import           Test.Tasty
import           Test.Tasty.HUnit



data TracePos = Subj | Comp Int

type TestTrace = (Text,Int,(TracePos,TraceChain Text),[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])

-- | silent pronoun
--
test_silent_pronoun :: TestTrace
test_silent_pronoun =
  ( "Republican senators plan to write a health-care bill."
  , 4,(Subj,TraceChain (Left (singletonLZ SilentPRO)) (Just "Republican senators"))
  , [(0,("republican","Republican")),(1,("senator","senators")),(2,("plan","plan")),(3,("to","to")),(4,("write","write")),(5,("a","a")),(6,("health-care","health-care")),(7,("bill","bill")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Republican"),PL ("NNS","senators")],PN "VP" [PL ("VBP","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","health-care"),PL ("NN","bill")]]]]],PL (".",".")]]
  , []
  )


-- | multi-level silent pronoun linking
--
test_multi_silent_pronoun :: TestTrace
test_multi_silent_pronoun =
  ( "I want to plan to write a paper."
  , 5, (Subj,TraceChain (Left (LZ [] SilentPRO [SilentPRO])) (Just "I"))
  , [(0,("I","I")),(1,("want","want")),(2,("to","to")),(3,("plan","plan")),(4,("to","to")),(5,("write","write")),(6,("a","a")),(7,("paper","paper")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","want"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","paper")]]]]]]]],PL (".",".")]]
  , []
  )

-- | relative WH-pronoun subject linking
--
test_relative_pronoun_subject :: TestTrace
test_relative_pronoun_subject =
  ( "I saw the man who sat on the bench."
  , 5, (Subj,TraceChain (Left (LZ [] Moved [WHPRO])) (Just "the man"))
  , [(0,("I","I")),(1,("see","saw")),(2,("the","the")),(3,("man","man")),(4,("who","who")),(5,("sit","sat")),(6,("on","on")),(7,("the","the")),(8,("bench","bench")),(9,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","saw"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","man")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBD","sat"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("DT","the"),PL ("NN","bench")]]]]]]],PL (".",".")]]
  , []
  )


-- | relative WH-pronoun object linking
--
test_relative_pronoun_object :: TestTrace
test_relative_pronoun_object =
  ( "I bought the book which Tim Cook read."
  , 7, (Comp 1,TraceChain (Left (LZ [] Moved [WHPRO])) (Just "the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("which","which")),(5,("Tim","Tim")),(6,("Cook","Cook")),(7,("read","read")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "NP" [PL ("NNP","Tim"),PL ("NNP","Cook")],PN "VP" [PL ("VBD","read")]]]]],PL (".",".")]]
  , []
  )



-- | reduced relative clause
--
test_reduced_relative_clause :: TestTrace
test_reduced_relative_clause =
  ( "I bought the book used by Chomsky."
  , 4, (Comp 1,TraceChain (Left (LZ [] Moved [Moved,WHPRO])) (Just "the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("use","used")),(5,("by","by")),(6,("Chomsky","Chomsky")),(7,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]]],PL (".",".")]]
  , []
  )


-- | passive verb
--
test_passive :: TestTrace
test_passive =
  ( "The book is used by Chomsky."
  , 3, (Comp 1, TraceChain (Left (LZ [] Moved [])) (Just "The book"))
  , [(0,("the","The")),(1,("book","book")),(2,("be","is")),(3,("use","used")),(4,("by","by")),(5,("Chomsky","Chomsky")),(6,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","book")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]],PL (".",".")]]
  , []
  )


--
-- | raising construction associated with passive ECM verb
--
test_passive_raising =
  ( "You are expected to call."
  , 4, (Subj,TraceChain (Left (LZ [] SilentPRO [Moved,Moved])) (Just "You"))
  , [(0,("you","You")),(1,("be","are")),(2,("expect","expected")),(3,("to","to")),(4,("call","call")),(5,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","You")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBN","expected"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","call")]]]]],PL (".",".")]]
  , []
  )

--
-- | ECM
--
test_ECM =
  ( "I expected him to call her."
  , 4, (Subj,TraceChain (Right []) (Just "him"))
  , [(0,("I","I")),(1,("expect","expected")),(2,("he","him")),(3,("to","to")),(4,("call","call")),(5,("she","her")),(6,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","expected"),PN "S" [PN "NP" [PL ("PRP","him")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","call"),PN "NP" [PL ("PRP","her")]]]]],PL (".",".")]]
  , []
  )


test_nonrestrictive_relative_clause =
  ( "Toyota said it would begin testing self-driving electric cars, which will use artificial intelligence to engage with drivers."
  , 12, (Subj,TraceChain (Left (LZ [] Moved [WHPRO])) (Just "self-driving electric cars"))
  , [(0,("Toyota","Toyota")),(1,("say","said")),(2,("it","it")),(3,("would","would")),(4,("begin","begin")),(5,("test","testing")),(6,("self-driving","self-driving")),(7,("electric","electric")),(8,("car","cars")),(9,(",",",")),(10,("which","which")),(11,("will","will")),(12,("use","use")),(13,("artificial","artificial")),(14,("intelligence","intelligence")),(15,("to","to")),(16,("engage","engage")),(17,("with","with")),(18,("driver","drivers")),(19,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Toyota")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","begin"),PN "S" [PN "VP" [PL ("VBG","testing"),PN "NP" [PN "NP" [PL ("JJ","self-driving"),PL ("JJ","electric"),PL ("NNS","cars")],PL (",",","),PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","use"),PN "NP" [PL ("JJ","artificial"),PL ("NN","intelligence")],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","engage"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNS","drivers")]]]]]]]]]]]]]]]]],PL (".",".")]]
  , []
  )



test_free_relative_clause_subject_1 =
  ( "President Donald Trump doesn't know who will be the next Fed chair."
  , 8, (Subj, TraceChain (Left (LZ [] Moved [WHPRO])) (Just "who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("will","will")),(8,("be","be")),(9,("the","the")),(10,("next","next")),(11,("Fed","Fed")),(12,("chair","chair")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")]]]]]]],PL (".",".")]]
  , []
  )

test_free_relative_clause_subject_2 =
  ( "President Donald Trump doesn't know who will be the next Fed chair."
  , 5, (Comp 1, TraceChain (Right [Moved,WHPRO]) (Just "who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("will","will")),(8,("be","be")),(9,("the","the")),(10,("next","next")),(11,("Fed","Fed")),(12,("chair","chair")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")]]]]]]],PL (".",".")]]
  , []
  )

test_free_relative_clause_object_1 =
  ( "President Donald Trump doesn't know who the next Fed chair will be."
  , 12, (Comp 1, TraceChain (Left (LZ [] Moved [WHPRO])) (Just "who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("the","the")),(8,("next","next")),(9,("Fed","Fed")),(10,("chair","chair")),(11,("will","will")),(12,("be","be")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be")]]]]]],PL (".",".")]]
  , []
  )

test_free_relative_clause_object_2 =
  ( "President Donald Trump doesn't know who the next Fed chair will be."
  , 5, (Comp 1, TraceChain (Right [Moved,WHPRO]) (Just "who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("the","the")),(8,("next","next")),(9,("Fed","Fed")),(10,("chair","chair")),(11,("will","will")),(12,("be","be")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be")]]]]]],PL (".",".")]]
  , []
  )



showDetail :: TestTrace -> IO ()
showDetail (txt,_,_,lma,pt,tagged) = mapM_ T.IO.putStrLn (formatDetail (txt,lma,pt,tagged))

{-
mainShow :: IO ()
mainShow = mapM_ showDetail [ test_silent_pronoun
                            , test_multi_silent_pronoun
                            , test_relative_pronoun_subject
                            , test_relative_pronoun_object
                            , test_reduced_relative_clause
                            ]
-}

testcases :: [TestTrace]
testcases = [ test_silent_pronoun
            , test_multi_silent_pronoun
            , test_relative_pronoun_subject
            , test_relative_pronoun_object
            , test_reduced_relative_clause
            , test_passive
            , test_passive_raising
            , test_ECM
            , test_nonrestrictive_relative_clause
            , test_free_relative_clause_subject_1
            , test_free_relative_clause_subject_2
            , test_free_relative_clause_object_1
            , test_free_relative_clause_object_2
            ]

checkTrace :: TestTrace -> Bool
checkTrace c =
  fromMaybe False $ do
    let --lmap1 = IM.fromList (map (_2 %~ (^._1)) (c^._4))
        --lemmapt = mkBitreeICP lmap1 (c^._5)
        tagged = mkTaggedLemma (c^._4) (c^._5) (c^._6)
        vps = mkVPS (c^._4) (c^._5)
        clausetr = clauseStructure tagged vps (bimap (\(rng,x) -> (rng,N.convert x)) id (mkPennTreeIdx (c^._5)))
        cpstr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps

    vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
    paws <- findPAWS tagged clausetr vp cpstr
    let cp = paws^.pa_CP
    case c^._3._1 of
      Subj   -> let dp = fmap (either (const "") (headText tagged)) (cp ^.complement.specifier)  -- for the time being. ignore CP subject
                in return (dp == c ^._3._2)
      Comp n -> do let comps = cp ^.complement.complement.complement
                   comp <- comps ^? ix (n-1)
                   let dp = fmap (compVPToHeadText tagged) comp
                   return (dp == c ^._3._2)





unitTests :: TestTree
unitTests = testGroup "Trace identification test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkTrace c @? (let (txt,_,_,lma,pt,tagged) = c in T.unpack (T.intercalate "\n" (formatDetail (txt,lma,pt,tagged))))
