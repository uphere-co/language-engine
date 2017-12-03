module Main where

{- 
complexNP_SRParser :: (Text,t
complexNP_SRParser
  = ( "SF Motors Inc, a California-based electric vehicle unit of China's Chongqing Sokon Industry Group Co Ltd, on Thursday said it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million."
    , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,(",",",")),(4,("a","a")),(5,("california-based","California-based")),(6,("electric","electric")),(7,("vehicle","vehicle")),(8,("unit","unit")),(9,("of","of")),(10,("China","China")),(11,("'s","'s")),(12,("Chongqing","Chongqing")),(13,("Sokon","Sokon")),(14,("Industry","Industry")),(15,("Group","Group")),(16,("Co","Co")),(17,("Ltd","Ltd")),(18,(",",",")),(19,("on","on")),(20,("Thursday","Thursday")),(21,("say","said")),(22,("it","it")),(23,("have","has")),(24,("buy","bought")),(25,("a","an")),(26,("ev","EV")),(27,("and","and")),(28,("battery","battery")),(29,("tech","tech")),(30,("firm","firm")),(31,("head","headed")),(32,("by","by")),(33,("former","former")),(34,("Tesla","Tesla")),(35,("Inc","Inc")),(36,("executive","executive")),(37,("Martin","Martin")),(38,("Eberhard","Eberhard")),(39,("for","for")),(40,("$","$")),(41,("33","33")),(42,("million","million")),(43,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","California-based"),PL ("JJ","electric"),PL ("NN","vehicle"),PL ("NN","unit")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNP","Chongqing"),PL ("NNP","Sokon"),PL ("NNP","Industry"),PL ("NNP","Group"),PL ("NNP","Co"),PL ("NNP","Ltd")]]],PL (",",",")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Thursday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","bought"),PN "NP" [PN "NP" [PL ("DT","an"),PL ("NN","EV"),PL ("CC","and"),PL ("NN","battery"),PL ("NN","tech"),PL ("NN","firm")],PN "VP" [PL ("VBN","headed"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("JJ","former"),PL ("NNP","Tesla"),PL ("NNP","Inc"),PL ("NN","executive"),PL ("NNP","Martin"),PL ("NNP","Eberhard")]]]],PN "PP" [PL ("IN","for"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","33"),PL ("CD","million")]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 3, MarkEntity Org), TagPos (TokIdx 20, TokIdx 21, MarkTime), TagPos (TokIdx 37,TokIdx 39, MarkEntity Person)]
    , []
    )
-}

main = do
  putStrLn "create performance testing set"
  
