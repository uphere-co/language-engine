{-# LANGUAGE OverloadedStrings #-}

module NLP.Type.UniversalDependencies2.Syntax where

import Data.Text (Text)

data DependencyRelation = ACL        -- ^ clausal modifier of noun (adjectival clause)
                        | ADVCL      -- ^ adverbial clause modifier
                        | ADVMOD     -- ^ adverbial modifier
                        | AMOD       -- ^ adjectival modifier
                        | APPOS      -- ^ appositional modifier
                        | AUX        -- ^ auxiliary
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
                        | EXPL       -- ^ expletive
                        | FIXED      -- ^ fixed multiword expression
                        | FLAT       -- ^ flat multiword expression
                        | GOESWITH   -- ^ goes with
                        | IOBJ       -- ^ indirect object
                        | LIST       -- ^ list
                        | MARK       -- ^ marker
                        | NMOD       -- ^ nominal modifier
                        | NSUBJ      -- ^ nominal subject 
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

parseDepRel :: Text -> Maybe DependencyRelation
parseDepRel "acl"        = Just ACL
parseDepRel "advcl"      = Just ADVCL
parseDepRel "advmod"     = Just ADVMOD
parseDepRel "amod"       = Just AMOD
parseDepRel "appos"      = Just APPOS
parseDepRel "aux"        = Just AUX
parseDepRel "case"       = Just CASE
parseDepRel "cc"         = Just CC
parseDepRel "ccomp"      = Just CCOMP
parseDepRel "clf"        = Just CLF
parseDepRel "compound"   = Just COMPOUND
parseDepRel "conj"       = Just CONJ
parseDepRel "cop"        = Just COP
parseDepRel "csubj"      = Just CSUBJ
parseDepRel "dep"        = Just DEP
parseDepRel "det"        = Just DET
parseDepRel "discourse"  = Just DISCOURSE
parseDepRel "dislocated" = Just DISLOCATED
parseDepRel "expl"       = Just EXPL
parseDepRel "fixed"      = Just FIXED
parseDepRel "flat"       = Just FLAT
parseDepRel "goeswith"   = Just GOESWITH
parseDepRel "iobj"       = Just IOBJ
parseDepRel "list"       = Just LIST
parseDepRel "mark"       = Just MARK
parseDepRel "nmod"       = Just NMOD
parseDepRel "nsubj"      = Just NSUBJ
parseDepRel "nummod"     = Just NUMMOD
parseDepRel "obj"        = Just OBJ
parseDepRel "obl"        = Just OBL
parseDepRel "orphan"     = Just ORPHAN
parseDepRel "parataxis"  = Just PARATAXIS
parseDepRel "punct"      = Just PUNCT
parseDepRel "reparandum" = Just REPARANDUM
parseDepRel "root"       = Just ROOT
parseDepRel "vocative"   = Just VOCATIVE
parseDepRel "xcomp"      = Just XCOMP
parseDepRel _            = Nothing
