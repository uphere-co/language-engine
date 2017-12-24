{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       (First(..))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.ListZipper
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos
import           WordNet.Type.Lexicographer        (LexicographerFile)
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                   (mkPreAnalysis)
--
import           Test.Common
import           Test.Tasty
import           Test.Tasty.HUnit
--
import Debug.Trace
import NLP.Syntax.Format



data TracePos = Subj | Comp Int

type TestTrace = (Text,Int,(TracePos,([TraceType],Text)),[(Int,(Lemma,Text))],PennTree
                 ,[TagPos TokIdx MarkType]
                 ,[(Int,LexicographerFile)]
                 )


--
-- | silent pronoun 1
--
test_silent_pronoun_1 :: TestTrace
test_silent_pronoun_1 =
  ( "Republican senators plan to write a health-care bill."
  , 4,(Subj, ([PRO],"Republican senators"))
  , [(0,("republican","Republican")),(1,("senator","senators")),(2,("plan","plan")),(3,("to","to")),(4,("write","write")),(5,("a","a")),(6,("health-care","health-care")),(7,("bill","bill")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Republican"),PL ("NNS","senators")],PN "VP" [PL ("VBP","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","health-care"),PL ("NN","bill")]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | silent pronoun 2
--
test_silent_pronoun_2 :: TestTrace
test_silent_pronoun_2 =
  ( "Toshiba Corp and Western Digital Corp have agreed in principle to settle a dispute."
  , 11, (Subj, ([PRO],"Toshiba Corp and Western Digital Corp"))
  , [(0,("Toshiba","Toshiba")),(1,("Corp","Corp")),(2,("and","and")),(3,("Western","Western")),(4,("Digital","Digital")),(5,("Corp","Corp")),(6,("have","have")),(7,("agree","agreed")),(8,("in","in")),(9,("principle","principle")),(10,("to","to")),(11,("settle","settle")),(12,("a","a")),(13,("dispute","dispute")),(14,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","Toshiba"),PL ("NNP","Corp")],PL ("CC","and"),PN "NP" [PL ("NNP","Western"),PL ("NNP","Digital"),PL ("NNP","Corp")]],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","agreed"),PN "PP" [PL ("IN","in"),PN "NP" [PL ("NN","principle")]],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","settle"),PN "NP" [PL ("DT","a"),PL ("NN","dispute")]]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | multi-level silent pronoun linking
--
test_multi_silent_pronoun :: TestTrace
test_multi_silent_pronoun =
  ( "I want to plan to write a paper."
  , 5, (Subj,([PRO,PRO],"I"))
  , [(0,("I","I")),(1,("want","want")),(2,("to","to")),(3,("plan","plan")),(4,("to","to")),(5,("write","write")),(6,("a","a")),(7,("paper","paper")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","want"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","paper")]]]]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | relative WH-pronoun subject linking
--
test_relative_pronoun_subject :: TestTrace
test_relative_pronoun_subject =
  ( "I saw the man who sat on the bench."
  , 5, (Subj,([Moved,WHPRO],"the man"))
  , [(0,("I","I")),(1,("see","saw")),(2,("the","the")),(3,("man","man")),(4,("who","who")),(5,("sit","sat")),(6,("on","on")),(7,("the","the")),(8,("bench","bench")),(9,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","saw"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","man")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBD","sat"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("DT","the"),PL ("NN","bench")]]]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | relative WH-pronoun object linking
--
test_relative_pronoun_object :: TestTrace
test_relative_pronoun_object =
  ( "I bought the book which Tim Cook read."
  , 7, (Comp 1,([Moved,WHPRO],"the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("which","which")),(5,("Tim","Tim")),(6,("Cook","Cook")),(7,("read","read")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "NP" [PL ("NNP","Tim"),PL ("NNP","Cook")],PN "VP" [PL ("VBD","read")]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | reduced relative clause
--
test_reduced_relative_clause :: TestTrace
test_reduced_relative_clause =
  ( "I bought the book used by Chomsky."
  , 4, (Comp 1,([Moved,Moved,WHPRO],"the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("use","used")),(5,("by","by")),(6,("Chomsky","Chomsky")),(7,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | passive verb
--
test_passive :: TestTrace
test_passive =
  ( "The book is used by Chomsky."
  , 3, (Comp 1, ([Moved],"The book"))
  , [(0,("the","The")),(1,("book","book")),(2,("be","is")),(3,("use","used")),(4,("by","by")),(5,("Chomsky","Chomsky")),(6,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","book")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | raising construction associated with passive ECM verb
--
test_passive_raising :: TestTrace
test_passive_raising =
  ( "You are expected to call."
  , 4, (Subj,([PRO,Moved,Moved],"You"))
  , [(0,("you","You")),(1,("be","are")),(2,("expect","expected")),(3,("to","to")),(4,("call","call")),(5,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","You")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBN","expected"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","call")]]]]],PL (".",".")]]
  , []
  , []
  )


--
-- | ECM
--
test_ECM :: TestTrace
test_ECM =
  ( "I expected him to call her."
  , 4, (Subj,([Moved],"him"))
  , [(0,("I","I")),(1,("expect","expected")),(2,("he","him")),(3,("to","to")),(4,("call","call")),(5,("she","her")),(6,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","expected"),PN "S" [PN "NP" [PL ("PRP","him")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","call"),PN "NP" [PL ("PRP","her")]]]]],PL (".",".")]]
  , []
  , []
  )


test_nonrestrictive_relative_clause :: TestTrace
test_nonrestrictive_relative_clause =
  ( "Toyota said it would begin testing self-driving electric cars, which will use artificial intelligence to engage with drivers."
  , 12, (Subj,([Moved,WHPRO],"self-driving electric cars"))
  , [(0,("Toyota","Toyota")),(1,("say","said")),(2,("it","it")),(3,("would","would")),(4,("begin","begin")),(5,("test","testing")),(6,("self-driving","self-driving")),(7,("electric","electric")),(8,("car","cars")),(9,(",",",")),(10,("which","which")),(11,("will","will")),(12,("use","use")),(13,("artificial","artificial")),(14,("intelligence","intelligence")),(15,("to","to")),(16,("engage","engage")),(17,("with","with")),(18,("driver","drivers")),(19,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Toyota")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","begin"),PN "S" [PN "VP" [PL ("VBG","testing"),PN "NP" [PN "NP" [PL ("JJ","self-driving"),PL ("JJ","electric"),PL ("NNS","cars")],PL (",",","),PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","use"),PN "NP" [PL ("JJ","artificial"),PL ("NN","intelligence")],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","engage"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNS","drivers")]]]]]]]]]]]]]]]]],PL (".",".")]]
  , []
  , []
  )


test_free_relative_clause_subject_1 :: TestTrace
test_free_relative_clause_subject_1 =
  ( "President Donald Trump doesn't know who will be the next Fed chair."
  , 8, (Subj, ([Moved,WHPRO],"who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("will","will")),(8,("be","be")),(9,("the","the")),(10,("next","next")),(11,("Fed","Fed")),(12,("chair","chair")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")]]]]]]],PL (".",".")]]
  , []
  , []
  )


test_free_relative_clause_subject_2 :: TestTrace
test_free_relative_clause_subject_2 =
  ( "President Donald Trump doesn't know who will be the next Fed chair."
  , 5, (Comp 1, ([Moved,WHPRO],"who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("will","will")),(8,("be","be")),(9,("the","the")),(10,("next","next")),(11,("Fed","Fed")),(12,("chair","chair")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")]]]]]]],PL (".",".")]]
  , []
  , []
  )


test_free_relative_clause_object_1 :: TestTrace
test_free_relative_clause_object_1 =
  ( "President Donald Trump doesn't know who the next Fed chair will be."
  , 12, (Comp 1, ([Moved,WHPRO],"who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("the","the")),(8,("next","next")),(9,("Fed","Fed")),(10,("chair","chair")),(11,("will","will")),(12,("be","be")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be")]]]]]],PL (".",".")]]
  , []
  , []
  )


test_free_relative_clause_object_2 :: TestTrace
test_free_relative_clause_object_2 =
  ( "President Donald Trump doesn't know who the next Fed chair will be."
  , 5, (Comp 1, ([Moved,WHPRO],"who"))
  , [(0,("President","President")),(1,("Donald","Donald")),(2,("Trump","Trump")),(3,("do","does")),(4,("not","n't")),(5,("know","know")),(6,("who","who")),(7,("the","the")),(8,("next","next")),(9,("Fed","Fed")),(10,("chair","chair")),(11,("will","will")),(12,("be","be")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBZ","does"),PL ("RB","n't"),PN "VP" [PL ("VB","know"),PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "NP" [PL ("DT","the"),PL ("JJ","next"),PL ("NNP","Fed"),PL ("NN","chair")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be")]]]]]],PL (".",".")]]
  , []
  , []
  )


test_topicalization_move :: TestTrace
test_topicalization_move
  = ( "Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile, Benjamin Steinbruch, chief executive officer, said on Friday."
    , 29, (Comp 2, ([Moved],"Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile"))
    , [(0,("brazilian","Brazilian")),(1,("steelmaker","steelmaker")),(2,("Companhia","Companhia")),(3,("Siderugica","Siderugica")),(4,("Nacional","Nacional")),(5,("SA","SA")),(6,("plan","plans")),(7,("to","to")),(8,("sell","sell")),(9,("bond","bonds")),(10,("on","on")),(11,("international","international")),(12,("market","markets")),(13,("in","in")),(14,("a","an")),(15,("effort","effort")),(16,("to","to")),(17,("improve","improve")),(18,("its","its")),(19,("debt","debt")),(20,("profile","profile")),(21,(",",",")),(22,("Benjamin","Benjamin")),(23,("Steinbruch","Steinbruch")),(24,(",",",")),(25,("chief","chief")),(26,("executive","executive")),(27,("officer","officer")),(28,(",",",")),(29,("say","said")),(30,("on","on")),(31,("Friday","Friday")),(32,(".","."))]
    , PN "ROOT" [PN "S" [PN "S" [PN "NP" [PL ("JJ","Brazilian"),PL ("NN","steelmaker"),PL ("NNP","Companhia"),PL ("NNP","Siderugica"),PL ("NNP","Nacional"),PL ("NNP","SA")],PN "VP" [PL ("VBZ","plans"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","sell"),PN "NP" [PN "NP" [PL ("NNS","bonds")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("JJ","international"),PL ("NNS","markets")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","an"),PL ("NN","effort"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","improve"),PN "NP" [PL ("PRP$","its"),PL ("NN","debt"),PL ("NN","profile")]]]]]]]]]]],PL (",",","),PN "NP" [PN "NP" [PL ("NNP","Benjamin"),PL ("NNP","Steinbruch")],PL (",",","),PN "NP" [PL ("JJ","chief"),PL ("NN","executive"),PL ("NN","officer")],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Friday")]]],PL (".",".")]]
    , []
    , []
    )


test_that_clause :: TestTrace
test_that_clause =
    ( "European shares lost some of the momentum that pushed stocks in Asia."
    , 8, (Subj, ([Moved,WHPRO],"the momentum"))
    , [(0,("european","European")),(1,("share","shares")),(2,("lose","lost")),(3,("some","some")),(4,("of","of")),(5,("the","the")),(6,("momentum","momentum")),(7,("that","that")),(8,("push","pushed")),(9,("stock","stocks")),(10,("in","in")),(11,("Asia","Asia")),(12,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","European"),PL ("NNS","shares")],PN "VP" [PL ("VBD","lost"),PN "NP" [PN "NP" [PL ("DT","some")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","momentum")],PN "SBAR" [PN "WHNP" [PL ("WDT","that")],PN "S" [PN "VP" [PL ("VBD","pushed"),PN "NP" [PL ("NNS","stocks")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("NNP","Asia")]]]]]]]]],PL (".",".")]]
    , []
    , []
    )



showDetail :: TestTrace -> IO ()
showDetail (txt,_,_,lma,pt,tagged,synsets) = mapM_ T.IO.putStrLn (formatDetail (txt,lma,pt,tagged,synsets))


testcases :: [TestTrace]
testcases = [ test_silent_pronoun_1
            , test_silent_pronoun_2
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
            , test_topicalization_move
            , test_that_clause
            ]


checkTrace :: TestTrace -> Bool
checkTrace c =
  fromMaybe False $ do
    let tagged = mkPreAnalysis (c^._4) (c^._5) (c^._6) (c^._7)
        vps = mkVPS (c^._4) (c^._5)
        x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps
    -- trace "checktrace1" $ return ()
    vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
    -- trace "checktrace2" $ return ()
    cp0 <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
                                             -- anyway need to be rewritten.
    -- trace "checktrace3" $ return ()

    cp <- (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById (cp0^.maximalProjection))) x'tr)
    -- trace ("checktrace4" ++ (formatCP cp)) $ return ()

    case c^._3._1 of
      Subj   -> do -- let dp = fmap (\case SpecTP_DP dp -> headTextDP tagged dp; _ -> "") (cp ^.complement.specifier)  -- for the time being. ignore CP subject
                   -- trace ("\ncheckTrace5:" ++ show dp) $ return ()
                   -- trace ( x'tr) $ return ()
                   -- trace ("\n" ++ T.unpack (T.intercalate "\n" (formatDetail (c^._1,c^._4,c^._5,c^._6,c^._7)))) $ return ()

                   -- trace ("\n" ++ (T.unpack . T.intercalate "\n" . map formatX'Tree) x'tr) $ return ()
                   -- return (dp == c ^._3._2)
                   return False
      Comp n -> do -- let comps = cp ^.complement.complement.complement
                   -- comp <- comps ^? ix (n-1)
                   -- let dp = fmap (compVPToHeadText tagged) comp
                   -- return (dp == c ^._3._2)
                   return False





unitTests :: TestTree
unitTests = testGroup "Trace identification test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkTrace c @? (let (txt,_,_,lma,pt,tagged,synsets) = c in T.unpack (T.intercalate "\n" (formatDetail (txt,lma,pt,tagged,synsets))))
