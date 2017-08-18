module Main where

import OntoNotes.App.Analyze (runAnalysis)
import OntoNotes.App.Load    (cfg)

main :: IO ()
main = runAnalysis cfg
