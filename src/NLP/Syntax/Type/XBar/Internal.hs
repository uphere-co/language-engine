{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}

module NLP.Syntax.Type.XBar.Internal where

import           Data.Text                   (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.ListZipper
import           Data.Range
--
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type.Verb


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data XType = X_V | X_T | X_C | X_D | X_P

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



-- | Denote trace. We use our own trace annotation system.
--   Empty categories are first identified as NULL and
--   will be resolved step by step.
--
data TraceType = NULL | SilentPRO | Moved | WHPRO
               deriving (Show,Eq,Ord)



-- | TraceChain represents the current focus of trace chain at a given position of tree.
--   Left case of trChain denodes the focus of the intermediate chain, while Right case 
--   of trChain denotes the case in which the final resolved element is currently focused.
--
--   e.g. (a -> b -> c -> d) is a trace chain.
--   * If a is focused, (Left ([],a,[b,c]),d)
--   * If b is focused, (Left ([a],b,[c]), d)
--   * If c is focused, (Left ([a,b],c,[]),d)
--   * If d is focused, (Right [a,b,c], d)
--
data TraceChain a = TraceChain { _trChain    :: Either (ListZipper TraceType) [TraceType]
                               , _trResolved :: Maybe a
                               }
                     deriving (Show,Eq,Ord,Functor)


emptyTraceChain :: TraceChain a
emptyTraceChain = TraceChain (Right []) Nothing



data SplitType = CLMod | BNMod | APMod
               deriving (Show,Eq,Ord)



--
-- this definition is not truly X-bar-theoretic, but for the time being
--
type instance Property   'X_D t = (Range,Range)   -- (original,head)
type instance Maximal    'X_D t = Zipper t
type instance Specifier  'X_D t = ()
type instance Adjunct    'X_D t = Maybe Range
type instance Complement 'X_D t = Maybe Range

type DetP = XP 'X_D


data Prep = Prep_NULL
          | Prep_WORD Text -- (Zipper t)

--
type instance Property   'X_P t = Prep
type instance Maximal    'X_P t = Zipper t
type instance Specifier  'X_P t = ()
type instance Adjunct    'X_P t = ()
type instance Complement 'X_P t = DetP t

type PP = XP 'X_P


data CompVP t = CompVP_Unresolved (Zipper t)
              | CompVP_CP (CP t)
              | CompVP_DP (DetP t)
              | CompVP_PP (PP t) -- (Maybe Text) (DetP t)



type instance Property   'X_V t = VerbProperty (Zipper t)
type instance Maximal    'X_V t = Zipper t
type instance Specifier  'X_V t = ()
type instance Adjunct    'X_V t = ()
type instance Complement 'X_V t = [TraceChain (CompVP t)]

type VerbP = XP 'X_V

mkVerbP :: Zipper t -> VerbProperty (Zipper t) -> [TraceChain (CompVP t)] -> VerbP t
mkVerbP vp vprop comps = XP vprop vp () () comps

type instance Property   'X_T t = ()
type instance Maximal    'X_T t = Zipper t
type instance Specifier  'X_T t = TraceChain (Either {- (CP t) -} (Zipper t) (DetP t))
type instance Adjunct    'X_T t = ()
type instance Complement 'X_T t = VerbP t

type TP = XP 'X_T

mkTP :: Zipper t -> TraceChain (Either {- (CP t) -} (Zipper t) (DetP t)) -> VerbP t -> TP t
mkTP tp mdp vp = XP () tp mdp () vp


data Complementizer t = C_PHI              -- ^ empty complementizer
                      | C_WORD (Zipper t)  -- ^ complementizer word
                      -- deriving (Show,Eq,Ord)


data SpecCP t = SpecCP_WHPHI           -- ^ empty Wh-word
              | SpecCP_WH (Zipper t)   -- ^ Wh-word



type instance Property   'X_C t = Complementizer t
type instance Maximal    'X_C t = Zipper t
type instance Specifier  'X_C t = Maybe (SpecCP t)
type instance Adjunct    'X_C t = ()
type instance Complement 'X_C t = TP t

type CP = XP 'X_C

mkCP :: Complementizer t -> Zipper t -> Maybe (SpecCP t) -> TP t -> CP t
mkCP mc cp spec tp = XP mc cp spec () tp


data CPDP a = CPCase (CP a)
            | DPCase (DetP a)
