{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Syntax.Type.XBar where

import           Control.Lens
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Range
--
import           Lexicon.Type
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type.Verb


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data XType = X_V | X_T | X_C

type family Property   (x :: XType) (tag :: [*]) :: *
 
type family Maximal    (x :: XType) (tag :: [*]) :: *
type family Specifier  (x :: XType) (tag :: [*]) :: *
type family Complement (x :: XType) (tag :: [*]) :: *
type family Adjunct    (x :: XType) (tag :: [*]) :: *

type Zipper tag = BitreeZipperICP tag

data XP x tag = XP { _headX             :: Property x tag
                   , _maximalProjection :: Maximal x tag
                   , _specifier         :: Specifier x tag
                   , _adjunct           :: Adjunct x tag
                   , _complement        :: Complement x tag
                   }


makeLenses ''XP

-- data DP a = SilentPRO | RExp a

-- | Denote trace. We use our own trace annotation system.
--   Empty categories are first identified as NULL and
--   will be resolved step by step.
--
data NTrace = NULL | SilentPRO | Moved | WHPRO  --    | Moved

type instance Property   'X_V t = VerbProperty (Zipper t)

type instance Maximal    'X_V t = Zipper t
type instance Specifier  'X_V t = ()
type instance Adjunct    'X_V t = ()
type instance Complement 'X_V t = [Zipper t]

type VerbP = XP 'X_V

mkVerbP :: Zipper t -> VerbProperty (Zipper t) -> [Zipper t] -> VerbP t
mkVerbP vp vprop comps = XP vprop vp () () comps

type instance Property   'X_T t = ()

type instance Maximal    'X_T t = Maybe (Zipper t)
type instance Specifier  'X_T t = [Either NTrace (Zipper t)] -- Maybe (ATNode (DP (Zipper t)))
type instance Adjunct    'X_T t = ()
type instance Complement 'X_T t = VerbP t

type TP = XP 'X_T

mkTP :: Maybe (Zipper t) -> [Either NTrace (Zipper t)] -> VerbP t -> TP t
mkTP mtp mdp vp = XP () mtp mdp () vp 

  
type instance Property   'X_C t = Maybe (Zipper t)   -- complementizer

type instance Maximal    'X_C t = Maybe (Zipper t)
type instance Specifier  'X_C t = ()
type instance Adjunct    'X_C t = ()
type instance Complement 'X_C t = TP t

type CP = XP 'X_C

mkCP :: Maybe (Zipper t) -> Maybe (Zipper t) -> TP t -> CP t
mkCP mcp mc tp = XP mc mcp () () tp

         
