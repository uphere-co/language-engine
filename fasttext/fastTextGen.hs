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
  , NonVirtual void_ "getVector" [cppclassref vector "vec", cppclassref string "word"] Nothing
  ]
  
vector :: Class
vector =
  Class cabal "Vector" [] mempty (Just "FastTextVector")
  [
    Constructor [ cint "m" ] Nothing
  , NonVirtual float_ "norm" [ ] Nothing
  ]
  
classes = [ string, fasttext, vector]
          
toplevelfunctions = []
                      
templates = []

headerMap = [ ("FastText", ([NS "fasttext", NS "std"], [HdrName "fasttext.h"]))
            , ("Vector"  , ([NS "fasttext"          ], [HdrName "/home/modori/repo/srcc/fastText/src/vector.h"  ]))
            , ("string"  , ([NS "std"               ], [HdrName "string"    ]))
            ]
            
main :: IO ()
main = do
  simpleBuilder "FastText.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ "fasttext" ] extraDep
