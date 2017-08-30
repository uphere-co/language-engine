{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Syntax.Type.XBar where

import Data.Attribute
import Data.Bitree
import Data.BitreeZipper
import Data.Range
import NLP.Type.PennTreebankII


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data XType = X_V | X_T | X_C

type family Property   (x :: XType) (tag :: [*]) :: *
 
type family Specifier  (x :: XType) (tag :: [*]) :: *
type family Complement (x :: XType) (tag :: [*]) :: *
type family Adjunct    (x :: XType) (tag :: [*]) :: *

type Zipper tag = BitreeZipperICP tag

data XP x tag = XP { _headX             :: Property x tag
                   , _maximalProjection :: Maybe (Zipper tag)
                   , _specifier         :: Specifier x tag
                   , _adjunct           :: Adjunct x tag
                   , _complement        :: Complement x tag
                   }


         
