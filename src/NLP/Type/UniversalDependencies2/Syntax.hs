module NLP.Type.UniversalDependencies2.Syntax where

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
