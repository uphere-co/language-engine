{-# LANGUAGE TemplateHaskell #-}

module SRL.Analyze.Config where


import           Control.Lens 

data Config = Config { _showDetail     :: Bool
                     , _showFullDetail :: Bool
                     , _bypassNER      :: Bool
                     , _configFile     :: FilePath
                     }
            deriving (Show)


makeLenses ''Config

