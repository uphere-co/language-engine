{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Trace where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import qualified Data.IntMap                as IM
import           Data.List                         (intercalate)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper                 (current,mkBitreeZipper)
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           NLP.Type.TagPos
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Preposition
import           NLP.Syntax.Util
import           NLP.Syntax.Verb
--
import           Test.Tasty.HUnit
import           Test.Tasty


-- | silent pronoun
--
test_silent_pronoun :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])
test_silent_pronoun =
  ( "Republican senators plan to write a health-care bill."
  , [(0,("republican","Republican")),(1,("senator","senators")),(2,("plan","plan")),(3,("to","to")),(4,("write","write")),(5,("a","a")),(6,("health-care","health-care")),(7,("bill","bill")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Republican"),PL ("NNS","senators")],PN "VP" [PL ("VBP","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","health-care"),PL ("NN","bill")]]]]],PL (".",".")]]
  , []
  )


-- | multi-level silent pronoun linking
--
test_multi_silent_pronoun :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])
test_multi_silent_pronoun =
  ( "I want to plan to write a paper."
  , [(0,("I","I")),(1,("want","want")),(2,("to","to")),(3,("plan","plan")),(4,("to","to")),(5,("write","write")),(6,("a","a")),(7,("paper","paper")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","want"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","paper")]]]]]]]],PL (".",".")]]
  , []
  )

-- | relative WH-pronoun subject linking
--
test_relative_pronoun_subject :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])
test_relative_pronoun_subject =
  ( "I saw the man who sat on the bench."
  , [(0,("I","I")),(1,("see","saw")),(2,("the","the")),(3,("man","man")),(4,("who","who")),(5,("sit","sat")),(6,("on","on")),(7,("the","the")),(8,("bench","bench")),(9,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","saw"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","man")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBD","sat"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("DT","the"),PL ("NN","bench")]]]]]]],PL (".",".")]]
  , []
  )


-- | relative WH-pronoun object linking
--
test_relative_pronoun_object :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])
test_relative_pronoun_object =
  ( "I bought the book which Tim Cook read."
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("which","which")),(5,("Tim","Tim")),(6,("Cook","Cook")),(7,("read","read")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "NP" [PL ("NNP","Tim"),PL ("NNP","Cook")],PN "VP" [PL ("VBD","read")]]]]],PL (".",".")]]
  , []
  )



-- | reduced relative clause
--
test_reduced_relative_clause :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])
test_reduced_relative_clause =
  ( "I bought the book used by Chomsky."
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("use","used")),(5,("by","by")),(6,("Chomsky","Chomsky")),(7,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]]],PL (".",".")]]
  , []
  )


formatDetail :: (Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)]) -> [Text]
formatDetail (txt,lma,pt,tmxs) =
  let lmap1 = IM.fromList (map (_2 %~ (\x -> Lemma (x^._1)))  lma)
      vps  = verbPropertyFromPennTree lmap1 pt
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx pt))
      mcpstr = (fmap (map bindingAnalysis) . identifyCPHierarchy tmxs) vps
      
  in   
  [ "--------------------------------------------------------------------------------------------------------------------"
  , txt
  , "--------------------------------------------------------------------------------------------------------------------"
  , (T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList) pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]
  ++ maybe [] (map formatCPHierarchy) mcpstr
  ++
  [ "--------------------------------------------------------------------------------------------------------------------"
  , prettyPrint 0 pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]
  {- ++ (flip concatMap tmxs $ \(TagPos (b,e,_tag)) -> 
       let lemmapt = mkBitreeICP lmap1 pt
           rng = beginEndToRange (b,e)
       in case find (\z -> getRoot (current z) ^? _Left . _1  == Just rng) $ getNodes (mkBitreeZipper [] lemmapt) of
            Nothing -> []
            Just z -> [T.pack (show (hasEmptyPreposition z))]
     )
  -}
  {- let vps = verbPropertyFromPennTree lmap1 pt
      mcpstr = identifyCPHierarchy vps
  case mcpstr of
    Nothing -> putStrLn "CP Hierarchy not identified..."
    Just cpstr -> do
      let fmt x = T.pack (show (x^._1))
      mapM_ (T.IO.putStrLn . linePrint fmt . toTree) cpstr
  -}


showDetail = mapM_ T.IO.putStrLn . formatDetail


mainShow :: IO ()
mainShow = mapM_ showDetail [ test_silent_pronoun
                            , test_multi_silent_pronoun
                            , test_relative_pronoun_subject
                            , test_relative_pronoun_object
                            , test_reduced_relative_clause
                            ]


testcases :: [(Text,[(Int,(Text,Text))],PennTree,[TagPos TokIdx (Maybe Text)])]
testcases = [ test_silent_pronoun
            , test_multi_silent_pronoun
            , test_relative_pronoun_subject
            , test_relative_pronoun_object
            , test_reduced_relative_clause
            ]


unitTests :: TestTree
unitTests = testGroup "Trace identification test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                {- (checkTrace c == True) -} False @? (T.unpack (T.intercalate "\n" (formatDetail c)))
