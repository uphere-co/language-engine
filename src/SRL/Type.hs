module SRL.Type where

data VArrow = Upward | Downward

data Deprel = ADV
            | AMOD
            | APPO
            | CONJ
            | COORD
            | DEP
            | DEP_GAP
            | DIR
            | DTV
            | EXT
            | EXTR
            | GAP_PRD
            | HMOD
            | HYPH
            | IM
            | LGS
            | LOC
            | LOC_PRD
            | MNR
            | NAME
            | NMOD
            | OBJ
            | OPRD
            | P 
            | PMOD
            | PRD
            | PRN
            | PRP
            | PRT                            
            | ROOT
            | SBJ
            | SUB
            | SUFFIX
            | TITLE
            | TMP
            | VOC
            | VC
            deriving (Show,Eq,Ord,Enum)


newtype DeprelPath = DeprelPath [(Deprel,VArrow)] 
