{-# LANGUAGE TemplateHaskell            #-}

module NLP.Syntax.Type.XBar.TH where

import           Control.Lens
--
import           NLP.Syntax.Type.XBar.Internal


makeLenses ''XP

makeLenses ''TraceChain

makePrisms ''CompVP

makePrisms ''Complementizer

makePrisms ''SpecCP

makePrisms ''CPDP

