module Main where

import OntoNotes.App.Analyze (runAnalysis)
import OntoNotes.App.Load    (cfg,cfgG)

main :: IO ()
main = runAnalysis cfgG
