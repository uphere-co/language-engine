module FrameNet.Type.PhraseType where

{-
data NullInstantiation = CNI
                       | DNI
                       | INI

data NounPhraseType = Poss -- ^ Possessive Noun Phrase
                    | N    -- ^ Non-Maximal Nominal
                    | NP   -- ^ Standard Noun Phrase

data PrepositionalPhraseType = PP         -- ^ Prepositional Phrases
                             | PPing      -- ^ Preposition with gerund object
                             | PPinterlog -- ^ Preposition governing a wh-interrogative clause
                             | PPadjP     -- ^ Preposition governing an adjective phrase

data VerbPhraseType = VPfin  -- ^ Finite Verb Phrase
                    | VPbrst -- ^ Bare Stem Verb Phrase
                    | VPto   -- ^ To-marked Infinitive Verb Phrase
                    | VPed   -- ^ Participal Verb Phrase
                    | VPing  -- ^ Gerundive Verb Phrase

data ClauseType = Sfin      -- ^ Finite Clause (with or withoug that)
                | Sinterrog -- ^ Wh-Clause
                | Swhether  -- ^ Whether/if-Clause
                | Sing      -- ^ Gerundive Clause
                | Sto       -- ^ To-marked Clause
                | Sforto    -- ^ For-to-marked Clause
                | Sbrst     -- ^ Bare Stem Clause
                | Sub       -- ^ Subordinate Clause with subordinating conjunction

data AdjAdvPhraseType = A   -- ^ Non-maximal Adjective
                      | AJP -- ^ Standard Adjective Phrase
 
data OtherType = QUO       -- ^ Quote
               | O_Dash    -- ^ Dash (--)
               | O_2nd     -- ^ 2nd
               | O_3rd     -- ^ 3rd
               | O_DEN     -- ^ DEN
               | O_INC     -- ^ INC
               | O_Num     -- ^ Num
               | O_Obj     -- ^ Obj
               | O_unknown -- ^ unknown


data PhraseType = PT_NI  NullInstantiation
                | PT_NP  NounPhraseType
                | PT_PP  PrepositionalPhraseType
                | PT_VP  VerbPhraseType
                | PT_C   ClauseType
                | PT_AJP AdjectivePhraseType
                | PT_AVP
                | PT_Other OtherType
                  
-}
