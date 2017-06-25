{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module WordNet.Type.Lexicographer where

import           Control.Lens
import           Data.Text
-- import qualified Data.Text as T
--
import           NLP.Type.WordNet


-- | I made a singleton pattern here for observing value and reflecting it to the type.
--   It has a kind of wierd name. It does not have AdjectiveSatellite case.
data SSSType (n :: SSType) where
  SNoun      :: SSSType 'Noun
  SVerb      :: SSSType 'Verb
  SAdjective :: SSSType 'Adjective
  SAdverb    :: SSSType 'Adverb


data PointerSymbol_Noun = PSN_Antonym                    --   "!"
                        | PSN_Hypernym                   --   "@"
                        | PSN_Instance_Hypernym          --   "@i"
                        | PSN_Hyponym                    --   "~"
                        | PSN_Instance_Hyponym           --   "~i"
                        | PSN_Member_Holohym             --   "#m"
                        | PSN_Substance_Holonym          --   "#s"
                        | PSN_Part_Holonym               --   "#p"
                        | PSN_Member_Meronym             --   "%m"
                        | PSN_Substance_Meronym          --   "%s"
                        | PSN_Part_Meronym               --   "%p"
                        | PSN_Attribute                  --   "="
                        | PSN_DerivationallyRelatedForm  --   "+"
                        | PSN_DomainOfSynset_TOPIC       --   ";c"
                        | PSN_MemberOfThisDomain_TOPIC   --   "-c"
                        | PSN_DomainOfSynset_REGION      --   ";r"
                        | PSN_MemberOfThisDomain_REGION  --   "-r"
                        | PSN_DomainOfSynset_USAGE       --   ";u"
                        | PSN_MemberOfThisDomain_USAGE   --   "-u"
                        deriving (Show,Eq,Ord)


data PointerSymbol_Verb = PSV_Antonym                    --   "!"
                        | PSV_Hypernym                   --   "@"
                        | PSV_Hyponym                    --   "~"
                        | PSV_Entailment                 --   "*"
                        | PSV_Cause                      --   ">"
                        | PSV_AlsoSee                    --   "^"
                        | PSV_VerbGroup                  --   "$"
                        | PSV_DerivationallyRelatedFrom  --   "+"
                        | PSV_DomainOfSynset_TOPIC       --   ";c"
                        | PSV_DomainOfSynset_REGION      --   ";r"
                        | PSV_DomainOfSynset_USAGE       --   ";u"
                        deriving (Show,Eq,Ord)


data PointerSymbol_Adjective = PSJ_Antonym               --   "!"
                             | PSJ_SimilarTo             --   "&"
                             | PSJ_ParticipleOfVerb      --   "<"
                             | PSJ_Pertainym             --   "\"  (pertains to noun)
                             | PSJ_Attribute             --   "="
                             | PSJ_AlsoSee               --   "^"
                             | PSJ_DomainOfSynset_TOPIC  --   ";c"
                             | PSJ_DomainOfSynset_REGION --   ";r"
                             | PSJ_DomainOfSynset_USAGE  --   ";u"
                             deriving (Show,Eq,Ord)
                                     

data PointerSymbol_Adverb = PSR_Antonym                  --   "!"
                          | PSR_DerviedFromAdjective     --   "\"
                          | PSR_DomainOfSynset_TOPIC     --   ";c" 
                          | PSR_DomainOfSynset_REGION    --   ";r"
                          | PSR_DomainOfSynset_USAGE     --   ";u"
                          deriving (Show,Eq,Ord)


type family PointerSymbol (a :: SSType) :: *

type instance PointerSymbol 'Noun      = PointerSymbol_Noun
type instance PointerSymbol 'Verb      = PointerSymbol_Verb
type instance PointerSymbol 'Adjective = PointerSymbol_Adjective
type instance PointerSymbol 'Adverb    = PointerSymbol_Adverb


data Marker = Marker_P  -- ^ predicate position
            | Marker_A  -- ^ prenominal (attributive) position
            | Marker_IP -- ^ immediately postnominal position
            deriving Show

data SSWord = SSWord { _ssw_word   :: [Text]
                     , _ssw_marker :: Maybe Marker
                     , _ssw_lexid  :: Maybe Int }
            deriving Show
                       
type Gloss = Text


data Synset a = Synset { _ssn_words    :: [SSWord]
                       , _ssn_pointers :: [PointerSymbol a]
                       , _ssn_frames   :: [Int]
                       , _ssn_gloss    :: Maybe Gloss }

makeLenses ''Synset


-- we need UndecidableInstances for this. 
instance (Show (PointerSymbol typ)) => Show (Synset typ) where
  show x = "Synset { _ssn_words=" ++ show (_ssn_words x)    ++
           ", _ssn_pointers="     ++ show (_ssn_pointers x) ++
           ", _ssn_frames="       ++ show (_ssn_frames x)   ++
           ", _ssn_gloss="        ++ show (_ssn_gloss x)    ++ "}"

