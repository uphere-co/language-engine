module Main where

import Foreign.C.String
--
import FastText.Binding.CppString
import FastText.Binding.FastText

main = do
  let binfile = "/scratch/wavewave/wordvector/wiki.en.bin"
  withCString binfile $ \cstr_bin -> do
    t <- newFastText
    str_bin <- newCppString cstr_bin
    fastTextloadModel t str_bin
    fastTextprintWordVectors t
