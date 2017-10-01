{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}

module NLP.Syntax.Type.XBar where

import           Control.Lens
import           Data.Foldable               (toList)
import           Data.Text                   (Text)
import qualified Data.Text              as T
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Range
--
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type.Verb


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data XType = X_V | X_T | X_C | X_D

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

-- | Denote trace. We use our own trace annotation system.
--   Empty categories are first identified as NULL and
--   will be resolved step by step.
--
data TraceType = NULL | SilentPRO | Moved | WHPRO
               deriving (Show,Eq,Ord)



data TraceChain a = TraceChain { _trChain    :: [TraceType]
                               , _trResolved :: Maybe a
                               }
                     deriving (Show,Eq,Ord,Functor)

makeLenses ''TraceChain

emptyTraceChain :: TraceChain a
emptyTraceChain = TraceChain [] Nothing


data CompVP a = CompVP_DP a
              | CompVP_PrepP (Maybe Text) a
              

makePrisms ''CompVP

uncoverCompVP :: CompVP a -> a
uncoverCompVP (CompVP_DP x) = x
uncoverCompVP (CompVP_PrepP _ x) = x


data SplitType = CLMod | BNMod | APMod
               deriving (Show,Eq,Ord)

getTokens :: BitreeICP as -> Text
getTokens = T.intercalate " " . map (tokenWord.snd) . toList


tokensByRange :: (Foldable t) => Range -> t (Int, ALeaf a) -> [Text]
tokensByRange rng = map snd . filter (^._1.to (\i -> i `isInside` rng)) . map (\(i,x)->(i,tokenWord x)) . toList


headRange x = x^.headX._2

headText x = (T.intercalate " " . tokensByRange (headRange x) . current) (x^.maximalProjection)


--
-- this definition is not truly X-bar-theoretic, but for the time being
--
type instance Property   'X_D t = (Range,Range)   -- (original,head)
type instance Maximal    'X_D t = Zipper t
type instance Specifier  'X_D t = ()
type instance Adjunct    'X_D t = Maybe Range
type instance Complement 'X_D t = Maybe Range

type DetP = XP 'X_D


type instance Property   'X_V t = VerbProperty (Zipper t)
type instance Maximal    'X_V t = Zipper t
type instance Specifier  'X_V t = ()
type instance Adjunct    'X_V t = ()
type instance Complement 'X_V t = [TraceChain (CompVP (DetP t))]

type VerbP = XP 'X_V

mkVerbP :: Zipper t -> VerbProperty (Zipper t) -> [TraceChain (CompVP (DetP t))] -> VerbP t
mkVerbP vp vprop comps = XP vprop vp () () comps

type instance Property   'X_T t = ()
type instance Maximal    'X_T t = Maybe (Zipper t)
type instance Specifier  'X_T t = TraceChain (DetP t)
type instance Adjunct    'X_T t = ()
type instance Complement 'X_T t = VerbP t

type TP = XP 'X_T

mkTP :: Maybe (Zipper t) -> TraceChain (DetP t) -> VerbP t -> TP t
mkTP mtp mdp vp = XP () mtp mdp () vp


data Complementizer t = C_PHI              -- ^ empty complementizer
                      | C_WORD (Zipper t)  -- ^ complementizer word
                      -- deriving (Show,Eq,Ord)

makePrisms ''Complementizer

data SpecCP t = SpecCP_WHPHI           -- ^ empty Wh-word
              | SpecCP_WH (Zipper t)   -- ^ Wh-word

makePrisms ''SpecCP


type instance Property   'X_C t = Complementizer t
type instance Maximal    'X_C t = Maybe (Zipper t)
type instance Specifier  'X_C t = Maybe (SpecCP t)
type instance Adjunct    'X_C t = ()
type instance Complement 'X_C t = TP t

type CP = XP 'X_C

mkCP :: Complementizer t -> Maybe (Zipper t) -> Maybe (SpecCP t) -> TP t -> CP t
mkCP mc mcp spec tp = XP mc mcp spec () tp


data CPDP a = CPCase (CP a)
            | DPCase (DetP a)

makePrisms ''CPDP
