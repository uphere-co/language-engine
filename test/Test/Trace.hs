{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Trace where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper
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
--
import           Test.Common
import           Test.Tasty.HUnit
import           Test.Tasty
--


data TracePos = Subj | Comp Int

type TestTrace = (Text,Int,(TracePos,TraceChain Text),[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])

-- | silent pronoun
--
test_silent_pronoun :: TestTrace
test_silent_pronoun =
  ( "Republican senators plan to write a health-care bill."
  , 4,(Subj,TraceChain [SilentPRO] (Just "Republican senators"))
  , [(0,("republican","Republican")),(1,("senator","senators")),(2,("plan","plan")),(3,("to","to")),(4,("write","write")),(5,("a","a")),(6,("health-care","health-care")),(7,("bill","bill")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Republican"),PL ("NNS","senators")],PN "VP" [PL ("VBP","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","health-care"),PL ("NN","bill")]]]]],PL (".",".")]]
  , []
  )


-- | multi-level silent pronoun linking
--
test_multi_silent_pronoun :: TestTrace
test_multi_silent_pronoun =
  ( "I want to plan to write a paper."
  , 5, (Subj,TraceChain [SilentPRO,SilentPRO] (Just "I"))    
  , [(0,("I","I")),(1,("want","want")),(2,("to","to")),(3,("plan","plan")),(4,("to","to")),(5,("write","write")),(6,("a","a")),(7,("paper","paper")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","want"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","paper")]]]]]]]],PL (".",".")]]
  , []
  )

-- | relative WH-pronoun subject linking
--
test_relative_pronoun_subject :: TestTrace
test_relative_pronoun_subject =
  ( "I saw the man who sat on the bench."
  , 5, (Subj,TraceChain [Moved,WHPRO] (Just "the man"))  
  , [(0,("I","I")),(1,("see","saw")),(2,("the","the")),(3,("man","man")),(4,("who","who")),(5,("sit","sat")),(6,("on","on")),(7,("the","the")),(8,("bench","bench")),(9,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","saw"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","man")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBD","sat"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("DT","the"),PL ("NN","bench")]]]]]]],PL (".",".")]]
  , []
  )


-- | relative WH-pronoun object linking
--
test_relative_pronoun_object :: TestTrace
test_relative_pronoun_object =
  ( "I bought the book which Tim Cook read."
  , 7, (Comp 1,TraceChain [Moved,WHPRO] (Just "the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("which","which")),(5,("Tim","Tim")),(6,("Cook","Cook")),(7,("read","read")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "NP" [PL ("NNP","Tim"),PL ("NNP","Cook")],PN "VP" [PL ("VBD","read")]]]]],PL (".",".")]]
  , []
  )



-- | reduced relative clause
--
test_reduced_relative_clause :: TestTrace
test_reduced_relative_clause =
  ( "I bought the book used by Chomsky."
  , 4, (Subj,TraceChain [Moved,WHPRO] (Just "the book"))
  , [(0,("I","I")),(1,("buy","bought")),(2,("the","the")),(3,("book","book")),(4,("use","used")),(5,("by","by")),(6,("Chomsky","Chomsky")),(7,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","bought"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","book")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("NNP","Chomsky")]]]]],PL (".",".")]]
  , []
  )


formatDetail :: TestTrace -> [Text]
formatDetail (_txt,_,_,lma,pt,tmxs) =
  let vps  = mkVPS lma pt
      clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx pt))
      mcpstr = (fmap (map bindingAnalysis) . identifyCPHierarchy tmxs) vps
 
  in   
  [ "===================================================================================================================="
  , (T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList) pt
  , "--------------------------------------------------------------------------------------------------------------------"
  ]
  ++ maybe [] (map formatCPHierarchy) mcpstr
  ++ map (formatVPwithPAWS tmxs clausetr mcpstr) vps
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


showDetail :: TestTrace -> IO ()
showDetail = mapM_ T.IO.putStrLn . formatDetail


mainShow :: IO ()
mainShow = mapM_ showDetail [ test_silent_pronoun
                            , test_multi_silent_pronoun
                            , test_relative_pronoun_subject
                            , test_relative_pronoun_object
                            , test_reduced_relative_clause
                            ]


testcases :: [TestTrace]
testcases = [ test_silent_pronoun
            , test_multi_silent_pronoun
            , test_relative_pronoun_subject
            , test_relative_pronoun_object
            , test_reduced_relative_clause
            ]

checkTrace :: TestTrace -> Bool
checkTrace c = False
  {- 
  fromMaybe False $ do
    let vps = mkVPS (c^._4) (c^._5)
        clausetr = clauseStructure vps (bimap (\(rng,x) -> (rng,N.convert x)) id (mkPennTreeIdx (c^._5)))
        mcpstr = (fmap (map bindingAnalysis) . identifyCPHierarchy (c^._6)) vps
    
    vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
    paws <- findPAWS [] clausetr vp mcpstr
    let cp = paws^.pa_CP
        gettokens = T.intercalate " " . map (tokenWord.snd) . toList . current
    case c^._3._1 of
      Subj   -> let dp = fmap gettokens (cp ^.complement.specifier)
                in return (dp == c ^._3._2)
      Comp n -> do let comps = cp ^.complement.complement.complement
                   comp <- comps ^? ix (n-1)
                   let dp = fmap (\case DP z -> gettokens z; PrepP _ z -> gettokens z) comp
                   return (dp == c ^._3._2)
-}


unitTests :: TestTree
unitTests = testGroup "Trace identification test" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                checkTrace c @? (T.unpack (T.intercalate "\n" (formatDetail c)))
