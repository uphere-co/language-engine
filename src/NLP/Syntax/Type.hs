{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module NLP.Syntax.Type where

import           Control.Lens
import           Data.Text                   (Text)
--
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N


type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst)) 

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst)) 


data Tense = Present | Past
           deriving (Show,Eq,Ord,Enum,Bounded)

                    
data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded)


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


data TP = TP { _tp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _tp_DP                 :: Maybe (BitreeZipperICP '[Lemma])
             , _tp_VP                 :: BitreeZipperICP '[Lemma]
             , _tp_verbProperty       :: VerbProperty (BitreeZipperICP '[Lemma])
             }

makeLenses ''TP


data CP = CP { _cp_maximal_projection :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_complementizer     :: Maybe (BitreeZipperICP '[Lemma])
             , _cp_TP                 :: TP
             }

makeLenses ''CP


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


