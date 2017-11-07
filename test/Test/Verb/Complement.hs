{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Verb.Complement where

import           Control.Lens               hiding (levels)
import qualified Data.IntMap                as IM
import           Data.List                         (find)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       (First(..),All(All,getAll),mconcat)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper                 (current,extractZipperById)
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.TagPos
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Type                   (MarkType(..),pa_CP)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                   (mkBitreeICP,mkTaggedLemma)
--
import           Test.Common
import           Test.Tasty.HUnit
import           Test.Tasty

import Debug.Trace


type TestVerbComplement = (Text
                          ,Int
                          ,((Text,Maybe NomClass),[Text],[Text])   -- (subject,complements,adjuncts)
                          ,[(Int,(Lemma,Text))]
                          ,PennTree,[TagPos TokIdx MarkType])

main_finite_1 :: TestVerbComplement
main_finite_1
  = ( "He will be fined $25,000.", 3
    , (("",Nothing), ["He","$ 25,000"],[])
    , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
    , []
    )

main_finite_2 :: TestVerbComplement
main_finite_2
  = ( "The move had been widely expected.", 5
    , (("",Nothing), ["The move"],[])
    , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
    , []
    )


-- | Reduced relative clause.
rrc_passive_1 :: TestVerbComplement
rrc_passive_1
  = ( "NASA enhances online scientific tool used by hundreds of scientists.", 5
    , (("by hundreds of scientists",Nothing), ["online scientific tool"],[])
    , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
    , []
    )


-- | control movement
inf_control_1 :: TestVerbComplement
inf_control_1
  = ( "Jean is reluctant to leave.", 4
    , (("Jean",Just RExp), [], [])
    , [(0,("Jean","Jean")),(1,("be","is")),(2,("reluctant","reluctant")),(3,("to","to")),(4,("leave","leave")),(5,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Jean")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("JJ","reluctant"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","leave")]]]]],PL (".",".")]]
    , []
    )

-- | embedded
embedded_that_1 :: TestVerbComplement
embedded_that_1
  = ( "The cat thinks that he is out of the bag.", 5
    , (("he",Just Pronoun), [], [])
    , [(0,("the","The")),(1,("cat","cat")),(2,("think","thinks")),(3,("that","that")),(4,("he","he")),(5,("be","is")),(6,("out","out")),(7,("of","of")),(8,("the","the")),(9,("bag","bag")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","cat")],PN "VP" [PL ("VBZ","thinks"),PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("IN","out"),PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","the"),PL ("NN","bag")]]]]]]],PL (".",".")]]
    , []
    )

-- | restrictive relative clause
restr_rel_1 :: TestVerbComplement
restr_rel_1
  = ( "The guy who is wearing the red hat just hit me!", 4
    , (("The guy",Just RExp), ["the red hat"], [])
    , [(0,("the","The")),(1,("guy","guy")),(2,("who","who")),(3,("be","is")),(4,("wear","wearing")),(5,("the","the")),(6,("red","red")),(7,("hat","hat")),(8,("just","just")),(9,("hit","hit")),(10,("I","me")),(11,("!","!"))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("DT","The"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","wearing"),PN "NP" [PL ("DT","the"),PL ("JJ","red"),PL ("NN","hat")]]]]]],PN "ADVP" [PL ("RB","just")],PN "VP" [PL ("VBD","hit"),PN "NP" [PL ("PRP","me")]],PL (".","!")]]
    , []
    )


ditransitive_1 :: TestVerbComplement
ditransitive_1
  = ( "I gave the guy an apple.", 1
    , (("I",Just Pronoun), ["the guy","an apple"], [])
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("a","an")),(5,("apple","apple")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "NP" [PL ("DT","an"),PL ("NN","apple")]],PL (".",".")]]
    , []
    )


ditransitive_2 :: TestVerbComplement
ditransitive_2
  = ( "I gave the guy what I got from her.", 1
    , (("I",Just Pronoun), ["the guy", "what I got from her"], [])
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("what","what")),(5,("I","I")),(6,("get","got")),(7,("from","from")),(8,("she","her")),(9,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WDT","what")],PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","got"),PN "PP" [PL ("IN","from"),PN "NP" [PL ("PRP","her")]]]]]]],PL (".",".")]]
    , []
    )


ditransitive_3 :: TestVerbComplement
ditransitive_3
  = ( "I told you that we would not pass the exam.", 1
    , (("I",Just Pronoun), ["you", "that we would not pass the exam"], [])
    , [(0,("I","I")),(1,("tell","told")),(2,("you","you")),(3,("that","that")),(4,("we","we")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PL ("PRP","you")],PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","we")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]],PL (".",".")]]
    , []
    )


ditransitive_4 :: TestVerbComplement
ditransitive_4
  = ( "I told the student he would not pass the exam.", 1
    , (("I",Just Pronoun), ["the student", "he would not pass the exam"], [] )
    , [(0,("I","I")),(1,("tell","told")),(2,("the","the")),(3,("student","student")),(4,("he","he")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","student")],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]]],PL (".",".")]]
    , []
    )

preposedTemporalAdjunct :: TestVerbComplement
preposedTemporalAdjunct
  = ( "Toyota Motor Corp on Monday said it would begin testing self-driving electric cars around 2020."
    , 5
    , (("Toyota Motor Corp",Just RExp), ["it would begin testing self-driving electric cars around 2020"], ["on Monday"])
    , [(0,("Toyota","Toyota")),(1,("Motor","Motor")),(2,("Corp","Corp")),(3,("on","on")),(4,("Monday","Monday")),(5,("say","said")),(6,("it","it")),(7,("would","would")),(8,("begin","begin")),(9,("test","testing")),(10,("self-driving","self-driving")),(11,("electric","electric")),(12,("car","cars")),(13,("around","around")),(14,("2020","2020")),(15,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","Toyota"),PL ("NNP","Motor"),PL ("NNP","Corp")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Monday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","begin"),PN "S" [PN "VP" [PL ("VBG","testing"),PN "NP" [PL ("JJ","self-driving"),PL ("JJ","electric"),PL ("NNS","cars")],PN "PP" [PL ("IN","around"),PN "NP" [PL ("CD","2020")]]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 4, TokIdx 5, MarkTime)]
    )


preposedCP :: TestVerbComplement
preposedCP
  = ( "Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile, Benjamin Steinbruch, chief executive officer, said on Friday."
    , 29
    , (("Benjamin Steinbruch",Just RExp), ["Friday", "Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile"], [])
    , [(0,("brazilian","Brazilian")),(1,("steelmaker","steelmaker")),(2,("Companhia","Companhia")),(3,("Siderugica","Siderugica")),(4,("Nacional","Nacional")),(5,("SA","SA")),(6,("plan","plans")),(7,("to","to")),(8,("sell","sell")),(9,("bond","bonds")),(10,("on","on")),(11,("international","international")),(12,("market","markets")),(13,("in","in")),(14,("a","an")),(15,("effort","effort")),(16,("to","to")),(17,("improve","improve")),(18,("its","its")),(19,("debt","debt")),(20,("profile","profile")),(21,(",",",")),(22,("Benjamin","Benjamin")),(23,("Steinbruch","Steinbruch")),(24,(",",",")),(25,("chief","chief")),(26,("executive","executive")),(27,("officer","officer")),(28,(",",",")),(29,("say","said")),(30,("on","on")),(31,("Friday","Friday")),(32,(".","."))]
    , PN "ROOT" [PN "S" [PN "S" [PN "NP" [PL ("JJ","Brazilian"),PL ("NN","steelmaker"),PL ("NNP","Companhia"),PL ("NNP","Siderugica"),PL ("NNP","Nacional"),PL ("NNP","SA")],PN "VP" [PL ("VBZ","plans"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","sell"),PN "NP" [PN "NP" [PL ("NNS","bonds")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("JJ","international"),PL ("NNS","markets")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","an"),PL ("NN","effort"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","improve"),PN "NP" [PL ("PRP$","its"),PL ("NN","debt"),PL ("NN","profile")]]]]]]]]]]],PL (",",","),PN "NP" [PN "NP" [PL ("NNP","Benjamin"),PL ("NNP","Steinbruch")],PL (",",","),PN "NP" [PL ("JJ","chief"),PL ("NN","executive"),PL ("NN","officer")],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Friday")]]],PL (".",".")]]
    , []
    )


complexNP :: TestVerbComplement
complexNP
  = ( "SF Motors Inc, a California-based electric vehicle unit of China's Chongqing Sokon Industry Group Co Ltd, on Thursday said it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million."
    , 21
    , (("SF Motors Inc",Just RExp), ["it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $ 33 million"], ["on Thursday"])
    , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,(",",",")),(4,("a","a")),(5,("california-based","California-based")),(6,("electric","electric")),(7,("vehicle","vehicle")),(8,("unit","unit")),(9,("of","of")),(10,("China","China")),(11,("'s","'s")),(12,("Chongqing","Chongqing")),(13,("Sokon","Sokon")),(14,("Industry","Industry")),(15,("Group","Group")),(16,("Co","Co")),(17,("Ltd","Ltd")),(18,(",",",")),(19,("on","on")),(20,("Thursday","Thursday")),(21,("say","said")),(22,("it","it")),(23,("have","has")),(24,("buy","bought")),(25,("a","an")),(26,("ev","EV")),(27,("and","and")),(28,("battery","battery")),(29,("tech","tech")),(30,("firm","firm")),(31,("head","headed")),(32,("by","by")),(33,("former","former")),(34,("Tesla","Tesla")),(35,("Inc","Inc")),(36,("executive","executive")),(37,("Martin","Martin")),(38,("Eberhard","Eberhard")),(39,("for","for")),(40,("$","$")),(41,("33","33")),(42,("million","million")),(43,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","California-based"),PL ("JJ","electric"),PL ("NN","vehicle"),PL ("NN","unit")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNP","Chongqing"),PL ("NNP","Sokon"),PL ("NNP","Industry"),PL ("NNP","Group"),PL ("NNP","Co"),PL ("NNP","Ltd")]]],PL (",",",")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Thursday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","bought"),PN "NP" [PN "NP" [PL ("DT","an"),PL ("NN","EV"),PL ("CC","and"),PL ("NN","battery"),PL ("NN","tech"),PL ("NN","firm")],PN "VP" [PL ("VBN","headed"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("JJ","former"),PL ("NNP","Tesla"),PL ("NNP","Inc"),PL ("NN","executive"),PL ("NNP","Martin"),PL ("NNP","Eberhard")],PN "PP" [PL ("IN","for"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","33"),PL ("CD","million")]]]]]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 20, TokIdx 21, MarkTime)]
    )

complexNP_2 :: TestVerbComplement
complexNP_2
  = ( "The event was the first initial public offering on the country's stock exchange."
    , 2
    , (("The event",Just RExp), ["the first initial public offering"],[])
    , [(0,("the","The")),(1,("event","event")),(2,("be","was")),(3,("the","the")),(4,("first","first")),(5,("initial","initial")),(6,("public","public")),(7,("offering","offering")),(8,("on","on")),(9,("the","the")),(10,("country","country")),(11,("'s","'s")),(12,("stock","stock")),(13,("exchange","exchange")),(14,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","event")],PN "VP" [PL ("VBD","was"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("JJ","first"),PL ("JJ","initial"),PL ("JJ","public"),PL ("NN","offering")],PN "PP" [PL ("IN","on"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","country"),PL ("POS","'s")],PL ("NN","stock"),PL ("NN","exchange")]]]],PL (".",".")]]
    , []
    )

prepComplement :: TestVerbComplement
prepComplement
  = ( "Virgin Group has invested in the company."
    , 3
    , (("Virgin Group",Just RExp), ["the company"], [])
    , [(0,("Virgin","Virgin")),(1,("Group","Group")),(2,("have","has")),(3,("invest","invested")),(4,("in","in")),(5,("the","the")),(6,("company","company")),(7,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Virgin"),PL ("NNP","Group")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","invested"),PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","the"),PL ("NN","company")]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 2, MarkTime)]
    )

{- 
formatTP :: TestVerbComplement -> [String]
formatTP (txt,i,_,lmatknlst,pt,taglst) =
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) lmatknlst)
      lemmapt = mkBitreeICP lmap1 pt
      tagged = TaggedLemma lemmapt lmatknlst taglst
      vps = mkVPS lmatknlst pt
      ipt = mkPennTreeIdx pt
      -- clausetr = clauseStructure tagged vps (bimap (\(rng,c) -> (rng,N.convert c)) id ipt)
      x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps
      cltxts = formatClauseStructure clausetr
  in case find (\vp -> vp^.vp_index == i) vps of
       Nothing -> [ "nothing"]
       Just vp -> [ "--------------------------------------------------------------"
                  , T.unpack txt
                  , T.unpack (prettyPrint 0 pt)
                  , printf "\nTesting Lemma: %2d-%-15s\nModal: %-15s"
                      (vp^.vp_index)
                      (vp^.vp_lemma.to unLemma)
                      (T.intercalate " " (vp^..vp_auxiliary.traverse._2._2.to unLemma))
                  ] ++ case (^.pa_CP) <$> findPAWS tagged clausetr vp x'tr of
                         Nothing -> ["not successful in constructing CP"]
                         Just cp -> [ formatCP cp
                                    , maybe "no subject?"  T.unpack (cp^?complement.specifier.trResolved._Just.to (either (getTokens . current) (headText tagged)))
                                    ]
                    ++ [T.unpack cltxts]
                    ++ map (T.unpack . formatX'Tree) x'tr


showTP :: TestVerbComplement -> IO ()
showTP = mapM_ putStrLn . formatTP


mainShow :: IO ()
mainShow = do
  showTP main_finite_1
  showTP main_finite_2
  showTP rrc_passive_1
  showTP inf_control_1
  showTP embedded_that_1
  showTP restr_rel_1
  showTP ditransitive_1
  showTP ditransitive_2
  showTP ditransitive_3
  showTP ditransitive_4
  showTP preposedTemporalAdjunct
  showTP preposedCP
-}

checkSubjCompAdjunct :: TestVerbComplement -> Bool
checkSubjCompAdjunct c = fromMaybe False $ do
  let tagged = mkTaggedLemma (c^._4) (c^._5) (c^._6)
      vps = mkVPS (c^._4) (c^._5)
      -- clausetr = clauseStructure tagged vps (bimap (\(rng,x) -> (rng,N.convert x)) id (mkPennTreeIdx (c^._5)))
      x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis tagged) . identifyCPHierarchy tagged) vps

  vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
  -- paws <- findPAWS tagged clausetr vp x'tr
  -- let cp = paws^.pa_CP
      -- test subjects
  cp0 <- (^._1) <$> constructCP tagged vp   -- seems very inefficient. but mcpstr can have memoized one.
                                             -- anyway need to be rewritten.
  cp <- (^? _CPCase) . currentCPDPPP =<< ((getFirst . foldMap (First . extractZipperById (cpRange cp0))) x'tr)
        
  let subj_test = c^._3._1
      b_subj = fromMaybe False $ do
                 subj <- cp^?complement.specifier.trResolved._Just
                 let sclass = subj^?_Right.headX.hd_class
                     stxt = either (getTokens . current) (headText tagged) subj
                 case subj_test^._2 of
                   Nothing -> return (stxt == subj_test^._1)
                   Just c -> return (stxt == subj_test^._1 && sclass == Just c)
      -- b_subj = subj == Just subj_test
      -- test complements
      -- lst_comps :: [Maybe Text]
      lst_comps = cp^..complement.complement.complement.traverse.trResolved.to (fmap (compVPToHeadText tagged))
      -- lst_comps_test :: [Text]
      lst_comps_test = c^._3._2
      b_comps = getAll (mconcat (zipWith (\a b -> All (a == Just b)) lst_comps lst_comps_test)) && (length lst_comps == length lst_comps_test)
      -- test adjuncts
      lst_adjs = cp^..complement.complement.adjunct.traverse.to (getTokens.current)
      lst_adjs_test = c^._3._3
      b_adjuncts = lst_adjs == lst_adjs_test
  return (b_subj && b_comps && b_adjuncts)


testcases :: [TestVerbComplement] -- [(Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)]
testcases = [ -- main_finite_1
              -- , main_finite_2
              -- , rrc_passive_1
              embedded_that_1
            , restr_rel_1
            , ditransitive_1
            -- , ditransitive_2
            , ditransitive_3
            -- , ditransitive_4
            , preposedTemporalAdjunct
            , preposedCP
            , complexNP
            , complexNP_2
            , prepComplement
            ]

unitTests :: TestTree
unitTests = testGroup "Subject and direct/indirect object identification" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                (checkSubjCompAdjunct c == True) @? (let (txt,_,_,lmatknlst,pt,tagged)=c in T.unpack (T.intercalate "\n" (formatDetail  (txt,lmatknlst,pt,tagged))))
