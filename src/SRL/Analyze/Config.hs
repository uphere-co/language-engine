{-# LANGUAGE TemplateHaskell #-}

module SRL.Analyze.Config where


import           Control.Lens 

data Config = Config { _showDetail :: Bool
                     , _showFullDetail :: Bool
                     }
            deriving (Show)


makeLenses ''Config
