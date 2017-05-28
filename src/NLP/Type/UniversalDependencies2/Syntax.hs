{-# LANGUAGE OverloadedStrings #-}

module NLP.Type.UniversalDependencies2.Syntax where

import           Data.Text      (Text)
import qualified Data.Text as T (unpack)

data DependencyRelation = ACL        -- ^ clausal modifier of noun (adjectival clause)
                        | ADVCL      -- ^ adverbial clause modifier
                        | ADVMOD     -- ^ adverbial modifier
                        | AMOD       -- ^ adjectival modifier
                        | APPOS      -- ^ appositional modifier
                        | AUX        -- ^ auxiliary
                        | AUXPASS    -- ^ auxiliary passive
                        | CASE       -- ^ case marking
                        | CC         -- ^ coordinating conjunction
                        | CCOMP      -- ^ clausal complement
                        | CLF        -- ^ classifier
                        | COMPOUND   -- ^ compound
                        | CONJ       -- ^ conjunct
                        | COP        -- ^ copula
                        | CSUBJ      -- ^ clausal subject
                        | DEP        -- ^ unspecified dependency 
                        | DET        -- ^ determiner
                        | DISCOURSE  -- ^ discourse element
                        | DISLOCATED -- ^ dislocated element
                        | DOBJ       -- ^ direct object 
                        | EXPL       -- ^ expletive
                        | FIXED      -- ^ fixed multiword expression
                        | FLAT       -- ^ flat multiword expression
                        | GOESWITH   -- ^ goes with
                        | IOBJ       -- ^ indirect object
                        | LIST       -- ^ list
                        | MARK       -- ^ marker
                        | MWE        -- ^ multiword expression
                        | NEG        -- ^ negation                     *
                        | NMOD       -- ^ nominal modifier
                        | NSUBJ      -- ^ nominal subject
                        | NSUBJPASS  -- ^ nominal subject passive
                        | NUMMOD     -- ^ numeric modifier
                        | OBJ        -- ^ object
                        | OBL        -- ^ oblique nominal
                        | ORPHAN     -- ^ orphan
                        | PARATAXIS  -- ^ parataxis
                        | PUNCT      -- ^ punctuation
                        | REPARANDUM -- ^ overridden disfluency
                        | ROOT       -- ^ root
                        | VOCATIVE   -- ^ vocative
                        | XCOMP      -- ^ open clausal complement
                        deriving (Show,Eq,Ord)

parseDepRel :: Text -> Either String DependencyRelation
parseDepRel "acl"        = Right ACL
parseDepRel "advcl"      = Right ADVCL
parseDepRel "advmod"     = Right ADVMOD
parseDepRel "amod"       = Right AMOD
parseDepRel "appos"      = Right APPOS
parseDepRel "aux"        = Right AUX
parseDepRel "auxpass"    = Right AUXPASS
parseDepRel "case"       = Right CASE
parseDepRel "cc"         = Right CC
parseDepRel "ccomp"      = Right CCOMP
parseDepRel "clf"        = Right CLF
parseDepRel "compound"   = Right COMPOUND
parseDepRel "conj"       = Right CONJ
parseDepRel "cop"        = Right COP
parseDepRel "csubj"      = Right CSUBJ
parseDepRel "dep"        = Right DEP
parseDepRel "det"        = Right DET
parseDepRel "discourse"  = Right DISCOURSE
parseDepRel "dislocated" = Right DISLOCATED
parseDepRel "dobj"       = Right DOBJ
parseDepRel "expl"       = Right EXPL
parseDepRel "fixed"      = Right FIXED
parseDepRel "flat"       = Right FLAT
parseDepRel "goeswith"   = Right GOESWITH
parseDepRel "iobj"       = Right IOBJ
parseDepRel "list"       = Right LIST
parseDepRel "mark"       = Right MARK
parseDepRel "mwe"        = Right MWE
parseDepRel "neg"        = Right NEG
parseDepRel "nmod"       = Right NMOD
parseDepRel "nsubj"      = Right NSUBJ
parseDepRel "nsubjpass"  = Right NSUBJPASS
parseDepRel "nummod"     = Right NUMMOD
parseDepRel "obj"        = Right OBJ
parseDepRel "obl"        = Right OBL
parseDepRel "orphan"     = Right ORPHAN
parseDepRel "parataxis"  = Right PARATAXIS
parseDepRel "punct"      = Right PUNCT
parseDepRel "reparandum" = Right REPARANDUM
parseDepRel "root"       = Right ROOT
parseDepRel "vocative"   = Right VOCATIVE
parseDepRel "xcomp"      = Right XCOMP
--
-- parseDepRel ""
parseDepRel x            = Left (T.unpack x)
