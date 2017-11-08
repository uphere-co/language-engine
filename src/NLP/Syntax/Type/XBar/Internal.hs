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
import           NLP.Type.TagPos             (TagPos,TokIdx)
--
import           NLP.Syntax.Type.Verb


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))

data MarkType = MarkTime | MarkEntity
              deriving (Show,Eq,Ord)

data TaggedLemma t = TaggedLemma { _pennTree  :: BitreeICP t
                                 , _lemmaList :: [(Int,(Lemma,Text))]
                                 , _tagList   :: [TagPos TokIdx MarkType]
                                 }

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


data CompDP t = CompDP_Unresolved Range
              | CompDP_CP (CP t)
              | CompDP_PP (PP t)


data AdjunctDP t = AdjunctDP_Unresolved Range
                 | AdjunctDP_PP (PP t)


data PronounType = P_I | P_You | P_He | P_She | P_It | P_They
                 deriving (Show,Eq,Ord)


identifyPronounType txt =
  case txt of
    "I"    -> Just P_I
    "me"   -> Just P_I
    "you"  -> Just P_You
    "he"   -> Just P_He
    "him"  -> Just P_He
    "she"  -> Just P_She
    "her"  -> Just P_She
    "it"   -> Just P_It
    "they" -> Just P_They
    "them" -> Just P_They
    _      -> Nothing

--
-- | noun class
--
data NomClass = RExp
              | Pronoun PronounType
              deriving (Show,Eq,Ord)


data HeadDP = HeadDP { _hd_range :: Range
                     , _hd_class :: NomClass
                     }


--
-- this definition is not truly X-bar-theoretic, but for the time being
--
type instance Property   'X_D t = HeadDP -- Range -- head
type instance Maximal    'X_D t = Range
type instance Specifier  'X_D t = Maybe Range  -- appositive for the time being
type instance Adjunct    'X_D t = [AdjunctDP t]
type instance Complement 'X_D t = Maybe (CompDP t)

type DetP = XP 'X_D

--
-- | These functions, mkOrdDP and mkSplittedDP, should be rewritten in a
--   better representation.
--
mkOrdDP :: Zipper t -> DetP t
mkOrdDP z = XP (HeadDP (rf z) RExp) (rf z) Nothing [] Nothing
  where rf = getRange . current


--
-- | This function is very ad hoc, should be eliminated.
--
mkSplittedDP :: SplitType
             -> Range        -- head
             -> Range
             -> Zipper t     -- zipper for maximal projection
             -> DetP t
mkSplittedDP typ h m o
  = case typ of
      CLMod -> XP (HeadDP h RExp) (rf o) Nothing   [] (Just (CompDP_Unresolved m))
      BNMod -> XP (HeadDP h RExp) (rf o) (Just m)  [] Nothing
      APMod -> XP (HeadDP h RExp) (rf o) (Just m)  [] Nothing -- apposition is regarded as an adjunct.
  where rf = getRange . current

--
-- | preposition word, including null preposition (for temporal expressions, numeral..)
--
data Prep = Prep_NULL
          | Prep_WORD Text
          deriving (Show,Eq,Ord)

--
-- | denote semantic class of preposition
--
data PrepClass = PC_Time
               | PC_Other
               deriving (Show,Eq,Ord)

--
-- | {Head,PP} has preposition word and semantic category.
--
data HeadPP = HeadPP { _hp_prep :: Prep
                     , _hp_pclass :: PrepClass }


data CompPP t = CompPP_DP (DetP t)
              | CompPP_Gerund (Zipper t)

--
type instance Property   'X_P t = HeadPP -- (Prep,PrepClass)
type instance Maximal    'X_P t = Range
type instance Specifier  'X_P t = ()
type instance Adjunct    'X_P t = ()
type instance Complement 'X_P t = CompPP t

type PP = XP 'X_P

mkPP :: (Prep,PrepClass) -> Range -> DetP t -> PP t
mkPP (prep,pclass) rng dp = XP (HeadPP prep pclass) rng () () (CompPP_DP dp)


mkPPGerund :: (Prep,PrepClass) -> Range -> Zipper t -> PP t
mkPPGerund (prep,pclass) rng z = XP (HeadPP prep pclass) rng () () (CompPP_Gerund z)

data CompVP t = CompVP_Unresolved (Zipper t)
              | CompVP_CP (CP t)
              | CompVP_DP (DetP t)
              | CompVP_PP (PP t)



type instance Property   'X_V t = VerbProperty (Zipper t)
type instance Maximal    'X_V t = Zipper t
type instance Specifier  'X_V t = ()
type instance Adjunct    'X_V t = [Zipper t]
type instance Complement 'X_V t = [TraceChain (CompVP t)]

type VerbP = XP 'X_V

mkVerbP :: Zipper t -> VerbProperty (Zipper t) -> [Zipper t] -> [TraceChain (CompVP t)] -> VerbP t
mkVerbP vp vprop adjs comps = XP vprop vp () adjs comps

type instance Property   'X_T t = ()
type instance Maximal    'X_T t = Zipper t
type instance Specifier  'X_T t = TraceChain (Either (Zipper t) (DetP t))
type instance Adjunct    'X_T t = ()
type instance Complement 'X_T t = VerbP t

type TP = XP 'X_T

mkTP :: Zipper t -> TraceChain (Either (Zipper t) (DetP t)) -> VerbP t -> TP t
mkTP tp mdp vp = XP () tp mdp () vp


data Complementizer t = C_PHI              -- ^ empty complementizer
                      | C_WORD (Zipper t)  -- ^ complementizer word
                      -- deriving (Show,Eq,Ord)


data SpecCP t = SpecCP_WHPHI           -- ^ empty Wh-word
              | SpecCP_WH (Zipper t)   -- ^ Wh-word
              | SpecCP_Topic (TraceChain (CompVP t)) -- ^ topicalization (AdjunctCP for the time being)



data AdjunctCP t = AdjunctCP_Unresolved (Zipper t)
                 | AdjunctCP_CP         (CP t)


type instance Property   'X_C t = Complementizer t
type instance Maximal    'X_C t = Zipper t
type instance Specifier  'X_C t = Maybe (SpecCP t)
type instance Adjunct    'X_C t = [AdjunctCP t]
type instance Complement 'X_C t = TP t

type CP = XP 'X_C

mkCP :: Complementizer t -> Zipper t -> Maybe (SpecCP t) -> [AdjunctCP t] -> TP t -> CP t
mkCP mc cp spec adjs tp = XP mc cp spec adjs tp


data CPDPPP t = CPCase (CP t)
              | DPCase (DetP t)
              | PPCase (PP t)


type X'Tree t = Bitree (Range,CPDPPP t) (Range,CPDPPP t)

type X'Zipper t = BitreeZipper (Range,CPDPPP t) (Range,CPDPPP t)
