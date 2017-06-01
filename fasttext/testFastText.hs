{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Vector.Storable
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
--
-- import FastText.Binding.CppString
-- import FastText.Binding.FastText
import FastText.Binding

foreign import ccall "get_vector" c_get_fasttextvector :: FastTextVector -> IO (Ptr CFloat)

main = do
  let binfile = "/scratch/wavewave/wordvector/wiki.en.bin"
  withCString binfile $ \cstr_bin -> do
    withCString "vector" $ \cstr_word -> do
      t <- newFastText
      v <- newFastTextVector 300
      str_bin <- newCppString cstr_bin
      str_word <- newCppString cstr_word
      fastTextloadModel t str_bin
      -- fastTextprintWordVectors t
      fastTextgetVector t v str_word
      print =<< fastTextVectornorm v
      c_size <- fastTextVectorsize v
      cfloat <- c_get_fasttextvector v
      ptr <- newForeignPtr_ cfloat
      let mv = MVector (fromIntegral c_size) ptr
      let v = create (return mv)

      print v
