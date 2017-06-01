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



classes = [ string, fasttext ] 

toplevelfunctions = []
    
templates = []

headerMap = [ ("FastText", ([NS "fasttext", NS "std"], [HdrName "fasttext.h"]))
            , ("string"  , ([NS "std"               ], [HdrName "string"    ]))
            ]

main :: IO ()
main = do 
  simpleBuilder "FastText.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ "fasttext" ] extraDep
