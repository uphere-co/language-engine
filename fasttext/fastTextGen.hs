module Main where

import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Reader
import Data.Monoid                (mempty)
import Data.Traversable           (sequenceA)
import System.Environment
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface




cabalgen acincs acsrcs =
  Cabal { cabal_pkgname = "fastText"
        , cabal_cheaderprefix = "FastText"
        , cabal_moduleprefix = "FastText.Binding"
        , cabal_additional_c_incs = acincs
        , cabal_additional_c_srcs = acsrcs
        , cabal_additional_pkgdeps = [ ]
        }

extraDep = []
  
cabalattr =
  CabalAttr
  { cabalattr_license = Just "BSD3"
  , cabalattr_licensefile = Just "LICENSE"
  , cabalattr_extraincludedirs = [ ]
  , cabalattr_extralibdirs = []
  , cabalattr_extrafiles = []
  }
  
string :: ReaderT Cabal IO Class
string = do
  cabal <- ask
  return $
    Class cabal "string" [] mempty  (Just "CppString")
      [ Constructor [ cstring "p" ] Nothing
      , NonVirtual cstring_ "c_str" [] Nothing
      ]

fasttext :: ReaderT Cabal IO Class
fasttext = do
  string' <- string
  vector' <- vector
  cabal <- ask 
  return $
    Class cabal "FastText" [] mempty Nothing
      [ Constructor [] Nothing
      , NonVirtual void_ "loadModel" [cppclassref string' "filename" ] Nothing
      , NonVirtual void_ "printWordVectors" [ ] Nothing
      , NonVirtual void_ "getVector" [cppclassref vector' "vec", cppclassref string' "word"] Nothing
      ]
  
vector :: ReaderT Cabal IO Class
vector = do
  cabal <- ask
  return $
    Class cabal "Vector" [] mempty (Just "FastTextVector")
    [ Constructor [ cint "m" ] Nothing
    , NonVirtual float_ "norm" [ ] Nothing
    , NonVirtual int_  "size" [ ] Nothing
    ]

classes :: ReaderT Cabal IO [Class]
classes = sequenceA [string,fasttext,vector]
  
toplevelfunctions = []
                      
templates = []

headerMap = [ ("FastText", ([NS "fasttext", NS "std"], [HdrName "fasttext.h"]))
            , ("Vector"  , ([NS "fasttext"          ], [HdrName "vectorwrapper.h"  ]))
            , ("string"  , ([NS "std"               ], [HdrName "string"    ]))
            ]
            
main :: IO ()
main = do
  args <- getArgs
  let file0 = args !! 0
      file1 = args !! 1
      
  cnts0 <- readFile file0
  cnts1 <- readFile file1
  let acincs = [ AddCInc "vectorwrapper.h" cnts0 ]
      acsrcs = [ AddCSrc "wrapper.cc" cnts1 ] 
  
  flip runReaderT (cabalgen acincs acsrcs) $ do
    cs <- classes
    cabal <- ask 
    lift $ simpleBuilder "FastText.Binding" headerMap (cabal,cabalattr,cs,toplevelfunctions,templates) [ "fasttext" ] extraDep
