module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = "fastText" 
              , cabal_cheaderprefix = "FastText"
              , cabal_moduleprefix = "FastText.Binding" }

extraDep = []

cabalattr = 
    CabalAttr 
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [ ]
    , cabalattr_extralibdirs = []
    , cabalattr_extrafiles = []
    }

{- 

ostream :: Class
ostream =
  Class cabal "ostream" [] mempty (Just "Ostream")
  [ 
  ]

wdict_entries :: Class
wdict_entries =
  Class cabal "WDict_entries" [] mempty Nothing
  [
  ]

wdict :: Class
wdict =
  Class cabal "WDict" [] mempty Nothing
  [ Static     (cppclassref_ wdict)         "instance"    [ ] Nothing
  , NonVirtual (cppclasscopy_ wdict_entries) "get_entries"
      [ cppclassref string "word", cppclassref string "pos" ] Nothing 
  ]

vectorfloatref_ = TemplateAppRef t_vector "CFloat" "std::vector<float>"

cword :: Class
cword =
  Class cabal "CWord" [] mempty Nothing
  [ NonVirtual (cppclasscopy_ string) "word" [] Nothing
  , NonVirtual (cppclasscopy_ string) "wpos" [] Nothing
  , NonVirtual (cppclasscopy_ string) "id" [] Nothing
  , NonVirtual (cppclasscopy_ string) "syn" [int "i"] Nothing
  ]

csentence :: Class
csentence =
  Class cabal "CSentence" [] mempty Nothing
  [ Constructor [ cstring "id" , cstring "ctx_str" ] Nothing
  , NonVirtual (cppclassref_ ostream) "print_csent" [ cppclassref ostream "o" ] Nothing
  , NonVirtual (cppclasscopy_ string) "id" [] Nothing        
  -- , NonVirtual (cppclassref_ csentenceConstIterator) "ubegin" [] Nothing
  -- , NonVirtual (cppclassref_ csentenceConstIterator) "uend" [] Nothing
  ]
-}

{- 
csentenceConstIterator :: Class
csentenceConstIterator =
  Class cabal "CSentence::const_iterator" [] mempty (Just "CSentenceConstIterator")
  [
  ]
-}

string :: Class 
string = 
  Class cabal "string" [] mempty  (Just "CppString")
  [ Constructor [ cstring "p" ] Nothing
  , NonVirtual cstring_ "c_str" [] Nothing 
  ]  


fasttext :: Class
fasttext =
  Class cabal "FastText" [] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "loadModel" [cppclassref string "filename" ] Nothing
  , NonVirtual void_ "printWordVectors" [ ] Nothing
  ]



classes = [ string, fasttext ] --  [ string,ostream,kb,wdict_entries,wdict,csentence,cword
             --  ]

toplevelfunctions = []
{- 
  [ TopLevelFunction bool_ "calculate_kb_ppr" [cppclassref csentence "cs", (vectorfloatref_, "ranks") ] Nothing  
  , TopLevelFunction bool_ "disamb_csentence_kb" [cppclassref csentence "cs", (vectorfloatref_, "ranks") ] Nothing  
  ]
-}



    
templates = []  -- [ (t_vector, HdrName "Vector.h") ]

headerMap = [ ("FastText", ([NS "fasttext", NS "std"], [HdrName "fasttext.h"]))
            , ("string"  , ([NS "std"               ], [HdrName "string"    ]))
            ]

main :: IO ()
main = do 
  simpleBuilder "FastText.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ "fasttext" ] extraDep
