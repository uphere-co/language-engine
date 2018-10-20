module Main where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM ( fromList )
import           Data.Monoid               ( mempty )
import           System.Environment        ( getArgs )
--
import           FFICXX.Generate.Builder
import           FFICXX.Generate.Code.Primitive
import           FFICXX.Generate.Type.Cabal( Cabal(..)
                                           , CabalName(..)
                                           , AddCInc(..)
                                           , AddCSrc(..)
                                           )
import           FFICXX.Generate.Type.Config
                                           ( ModuleUnit(..)
                                           , ModuleUnitMap(..)
                                           , ModuleUnitImports(..)
                                           )
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface

{-
import Data.Monoid                (mempty)
import Data.Traversable           (sequenceA)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface
-}

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------


-- import from stdcxx
stdcxx_cabal = Cabal { cabal_pkgname = CabalName "stdcxx"
                     , cabal_cheaderprefix = "STD"
                     , cabal_moduleprefix = "STD"
                     , cabal_additional_c_incs = []
                     , cabal_additional_c_srcs = []
                     , cabal_additional_pkgdeps = []
                     , cabal_pkg_config_depends = []
                     }

-- import from stdcxx
-- NOTE: inheritable class import needs method.
deletable :: Class
deletable =
  AbstractClass stdcxx_cabal "Deletable" [] mempty Nothing
    [ Destructor Nothing ]
    []
    []

-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ deletable ] mempty
  (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
  []
  []
  []


-- -------------------------------------------------------------------
-- fastText definition
-- -------------------------------------------------------------------


cabalgen acincs acsrcs =
  Cabal { cabal_pkgname = CabalName "fastText"
        , cabal_version = "0.0"
        , cabal_cheaderprefix = "FastText"
        , cabal_moduleprefix = "FastText.Binding"
        , cabal_additional_c_incs = acincs
        , cabal_additional_c_srcs = acsrcs
        , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
        , cabal_license = Just "BSD3"
        , cabal_licensefile = Just "LICENSE"
        , cabal_extraincludedirs = [ ]
        , cabal_extralibdirs = []
        , cabal_extrafiles = []
        , cabal_pkg_config_depends = []
        }

extraDep = []


fasttext :: ReaderT Cabal IO Class
fasttext = do
  vector' <- vector
  cabal <- ask
  return $
    Class cabal "FastText" [] mempty Nothing
      [ Constructor [] Nothing
      , NonVirtual void_ "loadModel" [cppclassref string "filename" ] Nothing
      , NonVirtual void_ "printWordVectors" [ ] Nothing
      , NonVirtual void_ "getVector" [cppclassref vector' "vec", cppclassref string "word"] Nothing
      ]
      []
      []

vector :: ReaderT Cabal IO Class
vector = do
  cabal <- ask
  return $
    Class cabal "Vector" [] mempty
    (Just (ClassAlias { caHaskellName = "FastTextVector", caFFIName = "Vector" }))
    [ Constructor [ cint "m" ] Nothing
    , NonVirtual float_ "norm" [ ] Nothing
    , NonVirtual int_  "size" [ ] Nothing
    ]
    [ ]
    [ ]


classes :: ReaderT Cabal IO [Class]
classes = sequenceA [fasttext,vector]

toplevelfunctions = []

templates = []

headerMap =
  ModuleUnitMap $
    HM.fromList $
      [ ( MU_Class "FastText"
        , ModuleUnitImports {
            muimports_namespaces = [ NS "fasttext", NS "std" ]
          , muimports_headers = [ HdrName "fasttext.h" ]
          }
        )
      , ( MU_Class "Vector"
        , ModuleUnitImports {
            muimports_namespaces = [ NS "fasttext" ]
          , muimports_headers = [ HdrName "vectorwrapper.h" ]
          }
        )
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
    lift $
      simpleBuilder
        "FastText.Binding"
        headerMap
        (cabal,cs,toplevelfunctions,templates)
        [ "fasttext" ]
        extraDep
