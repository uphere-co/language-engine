{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Verb.Complement where

import           Control.Lens               hiding (levels)
import           Data.Foldable                     (toList)
import           Data.List                         (find,intercalate)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       (All(All,getAll),mconcat)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Text.Printf
--
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated      as N
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
--
import           Test.Common
import           Test.Tasty.HUnit
import           Test.Tasty



main_finite_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
main_finite_1
  = ( "He will be fined $25,000.", 3
    , ("", ["He","$ 25,000"])
    , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
    )

main_finite_2 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
main_finite_2
  = ( "The move had been widely expected.", 5
    , ("", ["The move"])
    , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
    )


-- | Reduced relative clause.
rrc_passive_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
rrc_passive_1
  = ( "NASA enhances online scientific tool used by hundreds of scientists.", 5
    , ("by hundreds of scientists", ["online scientific tool"])
    , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
    )


-- | control movement
inf_control_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
inf_control_1
  = ( "Jean is reluctant to leave.", 4
    , ("Jean", [])
    , [(0,("Jean","Jean")),(1,("be","is")),(2,("reluctant","reluctant")),(3,("to","to")),(4,("leave","leave")),(5,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Jean")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("JJ","reluctant"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","leave")]]]]],PL (".",".")]]
    )

-- | embedded
embedded_that_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
embedded_that_1
  = ( "The cat thinks that he is out of the bag.", 5
    , ("he", [])
    , [(0,("the","The")),(1,("cat","cat")),(2,("think","thinks")),(3,("that","that")),(4,("he","he")),(5,("be","is")),(6,("out","out")),(7,("of","of")),(8,("the","the")),(9,("bag","bag")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","cat")],PN "VP" [PL ("VBZ","thinks"),PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("IN","out"),PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","the"),PL ("NN","bag")]]]]]]],PL (".",".")]]
    )

-- | restrictive relative clause
restr_rel_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
restr_rel_1
  = ( "The guy who is wearing the red hat just hit me!", 4
    , ("who", ["the red hat"])
    , [(0,("the","The")),(1,("guy","guy")),(2,("who","who")),(3,("be","is")),(4,("wear","wearing")),(5,("the","the")),(6,("red","red")),(7,("hat","hat")),(8,("just","just")),(9,("hit","hit")),(10,("I","me")),(11,("!","!"))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("DT","The"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","wearing"),PN "NP" [PL ("DT","the"),PL ("JJ","red"),PL ("NN","hat")]]]]]],PN "ADVP" [PL ("RB","just")],PN "VP" [PL ("VBD","hit"),PN "NP" [PL ("PRP","me")]],PL (".","!")]]
    )


ditransitive_1 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
ditransitive_1
  = ( "I gave the guy an apple.", 1
    , ("I", ["the guy","an apple"])
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("a","an")),(5,("apple","apple")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "NP" [PL ("DT","an"),PL ("NN","apple")]],PL (".",".")]]
    )


ditransitive_2 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
ditransitive_2
  = ( "I gave the guy what I got from her.", 1
    , ("I", ["the guy", "what I got from her"])
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("what","what")),(5,("I","I")),(6,("get","got")),(7,("from","from")),(8,("she","her")),(9,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WDT","what")],PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","got"),PN "PP" [PL ("IN","from"),PN "NP" [PL ("PRP","her")]]]]]]],PL (".",".")]]
    )


ditransitive_3 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
ditransitive_3
  = ( "I told you that we would not pass the exam.", 1
    , ("I", ["you", "that we would not pass the exam"])
    , [(0,("I","I")),(1,("tell","told")),(2,("you","you")),(3,("that","that")),(4,("we","we")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PL ("PRP","you")],PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","we")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]],PL (".",".")]]
    )


ditransitive_4 :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)
ditransitive_4
  = ( "I told the student he would not pass the exam.", 1
    , ("I", ["the student", "he would not pass the exam"])
    , [(0,("I","I")),(1,("tell","told")),(2,("the","the")),(3,("student","student")),(4,("he","he")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","student")],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]]],PL (".",".")]]
    )

            



formatTP :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree) -> [String]
formatTP (txt,i,_,lmatknlst,pt) =
  let vps = mkVPS lmatknlst pt
      ipt = mkPennTreeIdx pt
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id ipt)
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
                  ] ++ case constructCP [] vp of   -- for the time being
                         Nothing -> ["not successful in constructing CP"]
                         Just (cp,_) -> [formatCP cp]
                    ++ [T.unpack cltxts]

showTP :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree) -> IO ()
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


checkComplement :: (Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree) -> Bool
checkComplement c  = fromMaybe False $ do
  let vps = mkVPS (c^._4) (c^._5)
  vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
  (cp,_) <- constructCP [] vp  -- for the time being
  let gettokens x = case x of DP y -> getHeadTokens y; PrepP _ y -> getHeadTokens y
      --
      lst :: [Maybe Text]
      lst = cp^..complement.complement.complement.traverse.trResolved.to (fmap gettokens)
      --
      lst2 :: [Text]
      lst2 = c^._3._2
  return (getAll (mconcat (zipWith (\a b -> All (a == Just b)) lst lst2)) && (length lst == length lst2))


testcases :: [(Text,Int,(Text,[Text]),[(Int,(Lemma,Text))],PennTree)]
testcases = [ -- main_finite_1
              -- , main_finite_2
              -- , rrc_passive_1
              embedded_that_1
            , restr_rel_1
            , ditransitive_1
            -- , ditransitive_2
            , ditransitive_3
            -- , ditransitive_4
            ]

unitTests :: TestTree
unitTests = testGroup "Subject and direct/indirect object identification" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                (checkComplement c == True) @? (intercalate "\n" (formatTP c))

