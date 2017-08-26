{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.App.Analyze.Config where


import           Control.Lens 

data Config = Config { _showDetail :: Bool
                     , _showFullDetail :: Bool
                     }
            deriving (Show)


makeLenses ''Config

