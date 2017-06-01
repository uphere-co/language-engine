module Main where

import Foreign.C.String
--
-- import FastText.Binding.CppString
-- import FastText.Binding.FastText
import FastText.Binding

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
