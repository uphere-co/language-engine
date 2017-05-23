{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Type.PennTreebankII where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text      (Text)
import qualified Data.Text as T  
import           GHC.Generics

-- based on http://www.clips.ua.ac.be/pages/mbsp-tags
-- also http://www.surdeanu.info/mihai/teaching/ista555-fall13/readings/PennTreebankConstituents.html

data POSTag = CC          -- ^ conjunction, coordinating
            | CD          -- ^ cardinal number
            | DT          -- ^ determiner
            | EX          -- ^ existential there
            | FW          -- ^ foreign word 
            | IN          -- ^ conjunction, subordinating or preposition
            | JJ          -- ^ adjective
            | JJR         -- ^ adjective, comparative
            | JJS         -- ^ adjective, superlative
            | LS          -- ^ list item marker
            | MD          -- ^ verb, modal auxillary
            | NN          -- ^ noun, singular or mass
            | NNS         -- ^ noun, plural
            | NNP         -- ^ noun, proper singular
            | NNPS        -- ^ noun, proper plural
            | PDT         -- ^ predeterminer
            | POS         -- ^ possessive ending
            | PRP         -- ^ pronoun, personal
            | PRPDollar   -- ^ pronoun, possesive                (original PRP$)
            | RB          -- ^ adverb
            | RBR         -- ^ adverb, comparative
            | RBS         -- ^ adverb, superlative
            | RP          -- ^ adverb, particle
            | SYM         -- ^ symbol
            | TO          -- ^ infinitival to
            | UH          -- ^ interjection
            | VB          -- ^ verb, base form
            | VBZ         -- ^ verb, 3rd person singular present
            | VBP         -- ^ verb, non-3rd person singular present
            | VBD         -- ^ verb, past tense
            | VBN         -- ^ verb, past participle
            | VBG         -- ^ verb, gerund or present participle
            | WDT         -- ^ wh-determiner
            | WP          -- ^ wh-pronoun, personal
            | WPDollar    -- ^ wh-pronoun, possessive            (original WP$)
            | WRB         -- ^ wh-adverb
            | M_PERIOD    -- ^ punctuation mark, sentence closer (original .)
            | M_COMMA     -- ^ punctuation mark, comma           (original ,)
            | M_COLON     -- ^ punctuation mark, colon           (original :)
            | M_DQUOTE    -- ^ double quotation mark             (original '')
            | M_DBACKQUOTE -- ^ double back quotation mark       (original ``)
            | M_DOLLAR    -- ^ dollar sign                       (original $) 
            | D_LRB       -- ^ left parenthesis                  (original -LRB-)
            | D_RRB       -- ^ right parentheis                  (original -RRB-)
            | D_NONE      -- ^ none                              (original -NONE-)
            deriving (Generic, Show, Eq, Ord, Enum)

data ChunkTag = NP        -- ^ noun phrase
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
              | S         -- ^ sentence
              | SBAR      -- ^ subordinating conjunction
              | SBARQ     -- ^ direct question introduced by a wh-word or wh-phrase
              | SINV      -- ^ inverted declarative sentence
              | SQ        -- ^ inverted yes/no question
              deriving (Generic, Show,Eq,Ord,Enum)

data IOBPrefix = I_       -- ^ inside the chunk 
               | B_       -- ^ inside the chunk, preceding word is part of a different chunk
               | O_       -- ^ not part of a chunk
               deriving (Generic, Show,Eq,Ord,Enum) 

data RelationTag = R_SBJ  -- ^ sentence subject
                 | R_OBJ  -- ^ sentence object
                 | R_PRD  -- ^ predicate
                 | R_TMP  -- ^ temporal
                 | R_CLR  -- ^ closely related 
                 | R_LOC  -- ^ location
                 | R_DIR  -- ^ direction
                 | R_EXT  -- ^ extent 
                 | R_PRP  -- ^ purpose
                 deriving (Generic, Show,Eq,Ord,Enum)

data AnchorTag = A1       -- ^ anchor chunks that corresponds to P1
               | P1       -- ^ PNP that corresponds to A1
               deriving (Generic, Show,Eq,Ord,Enum)


identifyPOS :: Text -> POSTag
identifyPOS t
  | t == "-LRB-"  = D_LRB
  | t == "-RRB-"  = D_RRB
  | t == "-NONE-" = D_NONE
  | otherwise =
    let p = T.takeWhile (/= '-') t
    in if | p == "CC"   -> CC
          | p == "CD"   -> CD
          | p == "DT"   -> DT
          | p == "EX"   -> EX
          | p == "FW"   -> FW
          | p == "IN"   -> IN
          | p == "JJ"   -> JJ
          | p == "JJR"  -> JJR
          | p == "JJS"  -> JJS
          | p == "LS"   -> LS 
          | p == "MD"   -> MD
          | p == "NN"   -> NN 
          | p == "NNS"  -> NNS 
          | p == "NNP"  -> NNP
          | p == "NNPS" -> NNPS
          | p == "PDT"  -> PDT
          | p == "POS"  -> POS
          | p == "PRP"  -> PRP
          | p == "PRP$" -> PRPDollar 
          | p == "RB"   -> RB
          | p == "RBR"  -> RBR
          | p == "RBS"  -> RBS
          | p == "RP"   -> RP
          | p == "SYM"  -> SYM
          | p == "TO"   -> TO     
          | p == "UH"   -> UH     
          | p == "VB"   -> VB     
          | p == "VBZ"  -> VBZ    
          | p == "VBP"  -> VBP     
          | p == "VBD"  -> VBD    
          | p == "VBN"  -> VBN    
          | p == "VBG"  -> VBG    
          | p == "WDT"  -> WDT    
          | p == "WP"   -> WP     
          | p == "WP$"  -> WPDollar
          | p == "WRB"  -> WRB 
          | p == "."    -> M_PERIOD
          | p == ","    -> M_COMMA 
          | p == ":"    -> M_COLON 
          | p == "''"   -> M_DQUOTE
          | p == "``"   -> M_DBACKQUOTE
          | p == "$"    -> M_DOLLAR
          | otherwise   -> error ("invalid tag: " ++ T.unpack t)

identifyChunk :: Text -> ChunkTag
identifyChunk t =
    let p = T.takeWhile (/= '-') t
    in if | p == "NP"   -> NP
          | p == "PP"   -> PP
          | p == "VP"   -> VP
          | p == "ADVP" -> ADVP
          | p == "ADJP" -> ADJP
          | p == "PRT"  -> PRT
          | p == "INTJ" -> INTJ
          | p == "PNP"  -> PNP
          | p == "CONJP"-> CONJP  
          | p == "FRAG" -> FRAG 
          | p == "LST"  -> LST 
          | p == "NAC"  -> NAC
          | p == "NX"   -> NX  
          | p == "PRN"  -> PRN 
          | p == "QP"   -> QP  
          | p == "RRC"  -> RRC 
          | p == "UCP"  -> UCP 
          | p == "WHADJP" -> WHADJP
          | p == "WHADVP" -> WHADVP
          | p == "WHNP" -> WHNP
          | p == "WHPP" -> WHPP
          | p == "X"    -> X    
          | p == "S"    -> S
          | p == "SBAR" -> SBAR
          | p == "SBARQ" -> SBARQ
          | p == "SINV" -> SINV
          | p == "SQ"   -> SQ
          | otherwise   -> error ("no such chunk tag : " ++ T.unpack t)


instance FromJSON POSTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON POSTag where
  toJSON = genericToJSON defaultOptions

instance FromJSON ChunkTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChunkTag where
  toJSON = genericToJSON defaultOptions

instance FromJSON IOBPrefix where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON IOBPrefix where
  toJSON = genericToJSON defaultOptions

instance FromJSON RelationTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RelationTag where
  toJSON = genericToJSON defaultOptions

instance FromJSON AnchorTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON AnchorTag where
  toJSON = genericToJSON defaultOptions


data PennTreeGen c p a = PN c [PennTreeGen c p a]
                       | PL p a
                   deriving (Show, Functor, Foldable, Traversable)
                            
type PennTree = PennTreeGen Text Text Text




