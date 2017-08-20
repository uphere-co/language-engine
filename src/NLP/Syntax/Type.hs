{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Type where

import           Control.Lens
import           Data.Hashable                          (Hashable)
import           Data.Text                              (Text)
import           GHC.Generics                           (Generic)
--
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst))

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst))


data Tense = Present | Past
           deriving (Show,Eq,Ord,Enum,Bounded)


data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded,Read,Generic)

instance Hashable Voice


data Aspect = Simple | Progressive | Perfect | PerfectProgressive
           deriving (Show,Eq,Ord,Enum,Bounded)

data VerbProperty w = VerbProperty { _vp_index  :: Int
                                   , _vp_lemma  :: Lemma
                                   , _vp_tense  :: Tense
                                   , _vp_aspect :: Aspect
                                   , _vp_voice  :: Voice
                                   , _vp_auxiliary :: Maybe (w,(Int,Lemma))
                                   , _vp_negation :: Maybe (w,(Int,Lemma))
                                   , _vp_words  :: [(w,(Int,Lemma))]
                                   }
                    deriving (Show)

makeLenses ''VerbProperty

-- | Projection of Verb Phrase following X-bar theory.
--   The name VP is defined in NLP.Type.PennTreebankII, so I use VerbP.
--
data VerbP = VerbP { _vp_maximal_projection :: BitreeZipperICP '[Lemma]
                   , _vp_verbProperty       :: VerbProperty (BitreeZipperICP '[Lemma])
                   , _vp_complements        :: [BitreeZipperICP '[Lemma]]
                   }

makeLenses ''VerbP


-- | Projection of Tense Phrase following X-bar theory, which roughly
--   corresponds to a sentence.
--
data TP = TP { _tp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _tp_DP                 :: Maybe (BitreeZipperICP '[Lemma])
             , _tp_VP                 :: VerbP
             }

makeLenses ''TP

-- | Projection of Complementizer Phrase following X-bar theory
--
data CP = CP { _cp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_complementizer     :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_TP                 :: TP
             }

makeLenses ''CP


-----------------------------
-- ArgTable and ArgPattern --
-----------------------------



-- | ArgTable node that allows simple or linked node
-- 
data ATNode a = SimpleNode { _atnode_orig :: a }
              | LinkedNode { _atnode_orig  :: a
                           , _atnode_link  :: a } 
              deriving (Show,Functor,Foldable,Traversable)



-- now i can experiment flexibly with linked node
chooseATNode (SimpleNode x) = x
chooseATNode (LinkedNode x y) = x
                 
data ArgTable b = ArgTable { _tbl_rel  :: Maybe Text
                           , _tbl_arg0 :: Maybe b
                           , _tbl_arg1 :: Maybe b
                           , _tbl_arg2 :: Maybe b
                           , _tbl_arg3 :: Maybe b
                           , _tbl_arg4 :: Maybe b
                           , _tbl_file_sid_tid :: (FilePath,Int,Int)
                           }
                  deriving (Show,Functor,Foldable,Traversable)

makeLenses ''ArgTable



data ArgPattern a = ArgPattern { _patt_voice :: Maybe Voice
                               , _patt_arg0 :: Maybe a
                               , _patt_arg1 :: Maybe a
                               , _patt_arg2 :: Maybe a
                               , _patt_arg3 :: Maybe a
                               , _patt_arg4 :: Maybe a
                               }
                  deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)

makeLenses ''ArgPattern


instance Hashable (ArgPattern Text)


mkArgPattern :: Maybe TP -> ArgTable (ATNode a) -> ArgPattern a
mkArgPattern mtp ArgTable {..} = ArgPattern { _patt_voice = mtp^?_Just.tp_VP.vp_verbProperty.vp_voice
                                            , _patt_arg0 = fmap chooseATNode _tbl_arg0
                                            , _patt_arg1 = fmap chooseATNode _tbl_arg1
                                            , _patt_arg2 = fmap chooseATNode _tbl_arg2
                                            , _patt_arg3 = fmap chooseATNode _tbl_arg3
                                            , _patt_arg4 = fmap chooseATNode _tbl_arg4
                                            }


---------------
--           --
-- Old types --
--           --
---------------

data VerbArgs a = VerbArgs { _va_string :: [(POSTag,Text)]
                           , _va_arg0 :: Maybe a
                           , _va_args :: [a]
                           }
              deriving Show

makeLenses ''VerbArgs




data SBARType = SB_Word (POSTag,Text)
              | SB_WH   N.PhraseTag
              | SB_None
              deriving Show


data STag = S_RT
          | S_SBAR SBARType
          | S_CL N.ClauseTag
          | S_VP [(Int,(POSTag,Text))]
          | S_PP Text
          | S_OTHER N.PhraseTag
          deriving Show
