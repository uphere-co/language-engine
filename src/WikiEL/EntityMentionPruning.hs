{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WikiEL.EntityMentionPruning where

import qualified Data.Vector                   as V
import qualified WikiEL.EntityLinking          as EL

import           NLP.Type.PennTreebankII    (POSTag(..), TernaryLogic(..), isNoun, isVerb)
import           WikiEL.Misc                (IRange(..))
import           WikiEL.EntityLinking       (EntityMention(..))


isEntityLinkableTag :: POSTag -> Bool
isEntityLinkableTag = f . n -- || isVerb tag
  where
    n PRP = No
    n x   = isNoun x
    f Yes = True
    f _   = False

isEntityLinkable :: V.Vector POSTag -> IRange -> Bool
isEntityLinkable tags (IRange beg end) = V.any isEntityLinkableTag tokens
  where
    tokens = V.slice beg (end-beg) tags

filterEMbyPOS :: V.Vector POSTag -> [EntityMention w] -> [EntityMention w]
filterEMbyPOS wholeTags = filter f
  where
    f mention = isEntityLinkable wholeTags (EL.entityIRange mention)

filterEM = filterEMbyPOS
