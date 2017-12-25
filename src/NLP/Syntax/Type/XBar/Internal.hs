{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}

module NLP.Syntax.Type.XBar.Internal where

import           Control.Lens                ((^.))
import           Data.Text                   (Text)
--
import           Data.Bitree
import           Data.BitreeZipper
import           Data.Hashable
import           Data.ListZipper
import           Data.Range
import           GHC.Generics
--
import           NLP.Type.NamedEntity        (NamedEntityClass(..))
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.PreAnalysis

data Phase = P0 | P1

data XType = X_V
           | X_T
           | X_C
           | X_D
           | X_N
           | X_P
           | X_A


type Zipper tag = BitreeZipperICP tag




-- | Denote trace. We use our own trace annotation system.
--   Empty categories are first identified as NULL and
--   will be resolved step by step.
--
data TraceType = NULL | PRO | Moved | WHPRO
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

{- 
data TraceChain a = TraceChain { _trChain    :: Either (ListZipper TraceType) [TraceType]
                               , _trResolved :: Maybe a
                               }
                     deriving (Show,Eq,Ord,Functor)


emptyTraceChain :: TraceChain a
emptyTraceChain = TraceChain (Right []) Nothing
-}

-- type Coindex = Int

data Coindex a = Coindex { _coidx_i :: Maybe Int
                         , _coidx_content :: a }
               deriving (Show,Functor,Ord,Eq)

emptyCoindex = Coindex Nothing (Left NULL)

mkDefCoindex = Coindex Nothing

mkCoindex i = Coindex (Just i)

data SplitType = CLMod | BNMod | APMod
               deriving (Show,Eq,Ord)


data CompDP = CompDP_CP Range -- CP
            | CompDP_PP Range -- PP


data AdjunctDP = AdjunctDP_AP Range
               | AdjunctDP_PP Range
               deriving (Show,Eq,Ord,Generic)

instance Hashable AdjunctDP

data PronounPerson = P_I | P_You | P_He | P_She | P_It | P_They
                 deriving (Show,Eq,Ord)


-- data PronounCase = NomAcc | Genitive

identifyPronounPerson :: Text -> Maybe PronounPerson
identifyPronounPerson txt =
  case txt of
    "I"    -> Just P_I
    "me"   -> Just P_I
    "my"   -> Just P_I
    "mine" -> Just P_I
    "you"  -> Just P_You
    "your" -> Just P_You
    "yours"-> Just P_You
    "he"   -> Just P_He
    "him"  -> Just P_He
    "his"  -> Just P_He
    "she"  -> Just P_She
    "her"  -> Just P_She
    "hers" -> Just P_She
    "it"   -> Just P_It
    "its"  -> Just P_It
    "they" -> Just P_They
    "them" -> Just P_They
    "their"-> Just P_They
    "theirs" -> Just P_They
    _      -> Nothing


data Definiteness = Definite | Indefinite
                  deriving (Show,Eq)


identifyArticle :: Text -> Maybe DetClass
identifyArticle txt =
  case txt of
    "the" -> Just (Article Definite)
    "a"   -> Just (Article Indefinite)
    "an"  -> Just (Article Indefinite)
    -- this
    -- that
    -- these
    -- those
    _     -> Nothing

--
-- | noun class
--
data DetClass = NoDet
              | Pronoun PronounPerson Bool  -- is genitive?
              | Article Definiteness
              | GenitiveClitic
              deriving (Show,Eq)

-- type NomClass = Maybe NamedEntityClass

data HeadDP = HeadDP { _hd_range :: Maybe Range
                     , _hd_class :: DetClass
                     }



data SpecDP = SpDP_Appos Range
            | SpDP_Gen Range
            deriving (Show)


data HeadNP = HeadNP { _hn_range :: Range
                     , _hn_class :: Maybe NamedEntityClass -- NomClass
                     }
                     deriving (Show)



compDPToRange :: CompDP -> Range
compDPToRange (CompDP_CP cp) = cp
compDPToRange (CompDP_PP pp) = pp


mkNP :: (Range,Maybe NamedEntityClass) -> Maybe CompDP -> NounP
mkNP (rng,mclass) mcomp =
  case mcomp of
    Nothing -> XP (mkDefCoindex (HeadNP rng mclass)) rng () () Nothing
    Just comp -> let (b,_e) = rng
                     (b1,_e1) = compDPToRange comp
                     rng' = (b,b1-1)
                 in XP (mkDefCoindex (HeadNP rng' mclass)) rng () () (Just comp)



--
-- | These functions, mkOrdDP and mkSplittedDP, should be rewritten in a
--   better representation.
--
mkOrdDP :: Zipper t -> DetP
mkOrdDP z = XP (HeadDP Nothing NoDet) rng [] [] (Just (mkNP (rng,Nothing) Nothing))
  where rng = (getRange . current) z


--
-- | This function is very ad hoc, should be eliminated.
--
mkSplittedDP :: SplitType
             -> Range        -- head
             -> Range
             -> Zipper t     -- zipper for maximal projection
             -> DetP
mkSplittedDP typ h m o
  = case typ of
      CLMod -> XP (HeadDP Nothing NoDet) rng []             [] (Just (mkNP (h,Nothing) (Just (CompDP_CP m))))
      BNMod -> XP (HeadDP Nothing NoDet) rng [SpDP_Appos m] [] (Just (mkNP (h,Nothing) Nothing)) -- apposition is regarded as a specifier.
      APMod -> XP (HeadDP Nothing NoDet) rng [SpDP_Appos m] [] (Just (mkNP (h,Nothing) Nothing)) -- apposition is regarded as a specifier.
  where rng = (getRange . current) o

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


data CompPP = CompPP_DP DetP
            | CompPP_Gerund Range


mkPP :: (Prep,PrepClass) -> Range -> DetP -> PP
mkPP (prep,pclass) rng dp = XP (HeadPP prep pclass) rng () () (CompPP_DP dp)


mkPPGerund :: (Prep,PrepClass) -> Range -> Zipper t -> PP
mkPPGerund (prep,pclass) rng z = XP (HeadPP prep pclass) rng () () (CompPP_Gerund (getRange (current z)))




mkAP :: Range -> AP
mkAP rng = XP () rng () () ()

data CompVP = CompVP_Unresolved Range
            | CompVP_CP CP
            | CompVP_DP DetP
            | CompVP_PP PP
            | CompVP_AP AP


data AdjunctVP = AdjunctVP_Unresolved Range
               | AdjunctVP_PP PP



mkVerbP :: Range -> VerbProperty Text -> [AdjunctVP] -> [Coindex (Either TraceType CompVP)] -> VerbP
mkVerbP vp vprop adjs comps = XP vprop vp () adjs comps


data SpecTP = SpecTP_Unresolved Range
            | SpecTP_DP DetP



mkTP :: Range -> Coindex (Either TraceType SpecTP) -> VerbP -> TP
mkTP tp mdp vp = XP () tp mdp () vp


data Complementizer = C_PHI              -- ^ empty complementizer
                    | C_WORD Lemma       -- ^ complementizer lemma
                    -- deriving (Show,Eq,Ord)


data SpecCP = SpecCP_WHPHI                -- ^ empty Wh-word
            | SpecCP_WH Range --  (Coindex (Either TraceType DetP))   -- ^ Wh-phrase, this should be DP or PP. Later, we will change it to DP or PP.
            | SpecCP_Topic (Coindex (Either TraceType CompVP)) -- ^ topicalization (AdjunctCP for the time being)



data AdjunctCP = AdjunctCP_Unresolved Range
               | AdjunctCP_CP         CP



mkCP :: Complementizer -> Range -> Maybe SpecCP -> [AdjunctCP] -> TP -> CP
mkCP mc rng spec adjs tp = XP mc rng spec adjs tp


data CPDPPP = CPCase CP
            | DPCase DetP
            | PPCase PP
            | APCase AP

instance Show CPDPPP where
  show (CPCase cp) = "CP" ++ show (_maximalProjection cp)
  show (DPCase dp) = "DP" ++ show (_maximalProjection dp)
  show (PPCase pp) = "PP" ++ show (_maximalProjection pp)
  show (APCase ap) = "AP" ++ show (_maximalProjection ap)


type X'Tree = Bitree (Range,CPDPPP) (Range,CPDPPP)


type X'Zipper = BitreeZipper (Range,CPDPPP) (Range,CPDPPP)


data PPTree = PPTree PP (Maybe DPTree)


data DPTree = DPTree DetP [PPTree]


getSubsFromDPTree :: DPTree -> [CPDPPP]
getSubsFromDPTree (DPTree dp xs) = DPCase dp : (do PPTree pp my <- xs
                                                   PPCase pp : (maybe [] getSubsFromDPTree my))


getSubsFromPPTree :: PPTree -> [CPDPPP]
getSubsFromPPTree (PPTree pp my) = PPCase pp : maybe [] getSubsFromDPTree my





type family Property   (p :: Phase) (x :: XType) :: *
type family Maximal    (p :: Phase) (x :: XType) :: *
type family Specifier  (p :: Phase) (x :: XType) :: *
type family Complement (p :: Phase) (x :: XType) :: *
type family Adjunct    (p :: Phase) (x :: XType) :: *


data XP p x = XP { _headX             :: Property   p x
                 , _maximalProjection :: Maximal    p x
                 , _specifier         :: Specifier  p x
                 , _adjunct           :: Adjunct    p x
                 , _complement        :: Complement p x
                 }

  
--
-- NP
--
type instance Property   'P0 'X_N = Coindex HeadNP -- Range
type instance Maximal    'P0 'X_N = Range
type instance Specifier  'P0 'X_N = ()
type instance Adjunct    'P0 'X_N = ()
type instance Complement 'P0 'X_N = Maybe CompDP

type NounP = XP 'P0 'X_N


--
-- DP -> D NP
--
type instance Property   'P0 'X_D = HeadDP -- Range -- head
type instance Maximal    'P0 'X_D = Range
type instance Specifier  'P0 'X_D = [SpecDP] -- allow multiple spec for the time being
type instance Adjunct    'P0 'X_D = [AdjunctDP]
type instance Complement 'P0 'X_D = Maybe NounP -- Maybe (CompDP t)

type DetP = XP 'P0 'X_D



--
type instance Property   'P0 'X_P = HeadPP
type instance Maximal    'P0 'X_P = Range
type instance Specifier  'P0 'X_P = ()
type instance Adjunct    'P0 'X_P = ()
type instance Complement 'P0 'X_P = CompPP

type PP = XP 'P0 'X_P


type instance Property   'P0 'X_A = ()
type instance Maximal    'P0 'X_A = Range
type instance Specifier  'P0 'X_A = ()
type instance Adjunct    'P0 'X_A = ()
type instance Complement 'P0 'X_A = ()


type AP = XP 'P0 'X_A


type instance Property   'P0 'X_V = VerbProperty Text
type instance Maximal    'P0 'X_V = Range
type instance Specifier  'P0 'X_V = ()
type instance Adjunct    'P0 'X_V = [AdjunctVP]
type instance Complement 'P0 'X_V = [Coindex (Either TraceType CompVP)]

type VerbP = XP 'P0 'X_V


type instance Property   'P0 'X_T = ()
type instance Maximal    'P0 'X_T = Range
type instance Specifier  'P0 'X_T = Coindex (Either TraceType SpecTP)
type instance Adjunct    'P0 'X_T = ()
type instance Complement 'P0 'X_T = VerbP

type TP = XP 'P0 'X_T


type instance Property   'P0 'X_C = Complementizer
type instance Maximal    'P0 'X_C = Range
type instance Specifier  'P0 'X_C = Maybe SpecCP
type instance Adjunct    'P0 'X_C = [AdjunctCP]
type instance Complement 'P0 'X_C = TP

type CP = XP 'P0 'X_C
