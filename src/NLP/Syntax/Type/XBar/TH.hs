{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

module NLP.Syntax.Type.XBar.TH where

import           Control.Lens
--
import           NLP.Syntax.Type.XBar.Internal

makeLenses ''TaggedLemma

makeLenses ''XP

makeLenses ''TraceChain

-- makePrisms ''MaximalDP
makePrisms ''CompDP

makePrisms ''AdjunctDP

makePrisms ''CompPP

makePrisms ''Prep

makePrisms ''PrepClass

makePrisms ''CompVP

makePrisms ''Complementizer

makePrisms ''SpecCP

makePrisms ''AdjunctCP

makePrisms ''CPDPPP
