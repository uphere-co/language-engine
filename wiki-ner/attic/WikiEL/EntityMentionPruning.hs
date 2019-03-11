{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WikiEL.EntityMentionPruning where

import qualified Data.Vector                   as V
import qualified WikiEL.EntityLinking          as EL

import           NLP.Type.PennTreebankII    (POSTag(..), TernaryLogic(..), isNoun)
import           WikiEL.Type                (EntityMention,IRange(..))




filterEM :: V.Vector POSTag -> [EntityMention w] -> [EntityMention w]
filterEM = filterEMbyPOS
