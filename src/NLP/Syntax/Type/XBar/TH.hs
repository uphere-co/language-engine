{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

module NLP.Syntax.Type.XBar.TH where

import           Control.Lens
--
import           NLP.Syntax.Type.XBar.Internal

makePrisms ''MarkType

makeLenses ''TaggedLemma

makeLenses ''XP

makeLenses ''TraceChain

makePrisms ''DetClass
-- makePrisms ''NomClass

makeLenses ''HeadDP

makeLenses ''HeadNP

makePrisms ''SpecDP

makePrisms ''CompDP

makePrisms ''AdjunctDP


makePrisms ''Prep

makePrisms ''PrepClass

makeLenses ''HeadPP

makePrisms ''CompPP

makePrisms ''CompVP

makePrisms ''AdjunctVP

makePrisms ''SpecTP

makePrisms ''Complementizer

makePrisms ''SpecCP

makePrisms ''AdjunctCP

makePrisms ''CPDPPP
