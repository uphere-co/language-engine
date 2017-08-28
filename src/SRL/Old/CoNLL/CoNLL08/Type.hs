module SRL.Old.CoNLL.CoNLL08.Type where

data Deprel = ADV     -- ^ Unclassified adverbial
            | AMOD    -- ^ Modifier of adjective or adverb 
            | APPO    -- ^ Apposition
            | BNF     -- ^ Benefactor (the for phrase for verbs that undergo dative shift)
            | CONJ    -- ^ Between conjunction and second conjunct in a coordination
            | COORD   -- ^ Coordination
            | DEP     -- ^ Unclassified relation
            | DTV     -- ^ Dative (the to phrase for verbs that undergo dative shift)
            | EXTR    -- ^ Extraposed element in expletive constructions
            | GAP     -- ^ Gapping: between conjunction and the parts of a structure with an ellipsed head IM Between infinitive marker and verb
            | HMOD    -- ^ Modifier in hyphenation, such as two in two-part HYPH Between first part of hyphenation and hyphen NAME Name-internal link
            | LGS     -- ^ Logical subject
            | LOC     -- ^ Location
            | MNR     -- ^ Manner
            | NMOD    -- ^ Modifier of nominal
            | OBJ     -- ^ Direct or indirect object or clause complement
            | OPRD    -- ^ Object complement
            | P       -- ^ Punctuation
            | PMOD    -- ^ Between preposition and its child in a PP
            | POSTHON -- ^ Posthonorifics such as Jr, Inc.
            | PRD     -- ^ Predicative complement
            | PRN     -- ^ Parenthetical
            | PRP     -- ^ Purpose or reason
            | PRT     -- ^ Particle
            | PUT     -- ^ Various locative complements of the verb put  * 
            | ROOT    -- ^ Root
            | SBJ     -- ^ Subject
            | SUB     -- ^ Between subordinating conjunction and verb
            | SUFFIX  -- ^ Possessive ’s
            | TITLE   -- ^ Titles such as Mr, Dr
            | TMP     -- ^ Temporal
            | VC      -- ^ Verb chain
            | VOC     -- ^ Vocative
            -- 
            | ADV_GAP
            | AMOD_GAP
            | DEP_GAP
            | DIR     -- ^ Direction
            | DIR_GAP
            | DTV_GAP
            | END
            | EXT     -- ^ Extent              
            | EXT_GAP
            | EXTR_GAP
            | GAP_LGS
            | GAP_LOC
            | GAP_LOC_PRD
            | GAP_MNR
            | GAP_NMOD
            | GAP_OBJ
            | GAP_OPRD
            | GAP_PMOD
            | GAP_PRD
            | GAP_PRP
            | GAP_PUT
            | GAP_SBJ
            | GAP_SUB
            | GAP_TMP
            | GAP_VC
            | HYPH
            | IM
            | LOC_MNR
            | LOC_OPRD
            | LOC_PRD
            | LOC_TMP
            | MNR_PRD
            | MNR_TMP
            | NAME
            | PRD_PRP
            | PRD_TMP  
            

            deriving (Show,Eq,Ord,Enum)
