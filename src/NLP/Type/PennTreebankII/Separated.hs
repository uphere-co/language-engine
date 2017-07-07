{-# LANGUAGE DeriveGeneric #-}

module NLP.Type.PennTreebankII.Separated where

import           GHC.Generics
import qualified NLP.Type.PennTreebankII as O

data PhraseTag = NP        -- ^ noun phrase
               | PP        -- ^ prepositional phrase
               | VP        -- ^ verb phrase
               | ADVP      -- ^ adverb phrase
               | ADJP      -- ^ adjective phrase
               | PRT       -- ^ particle 
               | INTJ      -- ^ interjection
               | PNP       -- ^ prepositional noun phrase
               | CONJP     -- ^ conjunction phrase
               | FRAG      -- ^ fragment
               | LST       -- ^ list marker
               | NAC       -- ^ not a constituent
               | NX        -- ^ mark the head of the NP
               | PRN       -- ^ parenthetical
               | QP        -- ^ quantifier phrase
               | RRC       -- ^ reduced relative clause                 
               | UCP       -- ^ unlike coordnated phrase
               | WHADJP    -- ^ wh-adjective phrase
               | WHADVP    -- ^ wh-adverb phrase
               | WHNP      -- ^ wh-noun phrase
               | WHPP      -- ^ wh-prepositional phrase
               | X         -- ^ unknown
               deriving (Generic,Show,Eq,Ord,Enum,Bounded)


data ClauseTag = S         -- ^ sentence
               | SBAR      -- ^ subordinating conjunction
               | SBARQ     -- ^ direct question introduced by a wh-word or wh-phrase
               | SINV      -- ^ inverted declarative sentence
               | SQ        -- ^ inverted yes/no question
               deriving (Generic, Show,Eq,Ord,Enum,Bounded)


data CombinedTag = CL ClauseTag
                 | PH PhraseTag
                 | RT
              deriving (Generic,Show,Eq,Ord)


convert :: O.ChunkTag -> CombinedTag
convert O.ROOT   = RT
convert O.S      = CL S
convert O.SBAR   = CL SBAR
convert O.SBARQ  = CL SBARQ
convert O.SINV   = CL SINV
convert O.SQ     = CL SQ
convert O.NP     = PH NP
convert O.PP     = PH PP 
convert O.VP     = PH VP
convert O.ADVP   = PH ADVP
convert O.ADJP   = PH ADJP
convert O.PRT    = PH PRT
convert O.INTJ   = PH INTJ
convert O.PNP    = PH PNP
convert O.CONJP  = PH CONJP
convert O.FRAG   = PH FRAG
convert O.LST    = PH LST
convert O.NAC    = PH NAC
convert O.NX     = PH NX
convert O.PRN    = PH PRN
convert O.QP     = PH QP
convert O.RRC    = PH RRC
convert O.UCP    = PH UCP
convert O.WHADJP = PH WHADJP
convert O.WHADVP = PH WHADVP
convert O.WHNP   = PH WHNP
convert O.WHPP   = PH WHPP
convert O.X      = PH X
