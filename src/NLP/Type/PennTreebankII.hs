{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Type.PennTreebankII where

import           Control.Monad.Trans.State (evalState,get,put)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable         (toList)
import           Data.Text             (Text)
import qualified Data.Text        as T  
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

instance FromJSON POSTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON POSTag where
  toJSON = genericToJSON defaultOptions


data ChunkTag = ROOT
              | NP        -- ^ noun phrase
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

instance FromJSON ChunkTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChunkTag where
  toJSON = genericToJSON defaultOptions


data IOBPrefix = I_       -- ^ inside the chunk 
               | B_       -- ^ inside the chunk, preceding word is part of a different chunk
               | O_       -- ^ not part of a chunk
               deriving (Generic, Show,Eq,Ord,Enum) 

instance FromJSON IOBPrefix where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON IOBPrefix where
  toJSON = genericToJSON defaultOptions


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

instance FromJSON RelationTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RelationTag where
  toJSON = genericToJSON defaultOptions


data AnchorTag = A1       -- ^ anchor chunks that corresponds to P1
               | P1       -- ^ PNP that corresponds to A1
               deriving (Generic, Show,Eq,Ord,Enum)

instance FromJSON AnchorTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON AnchorTag where
  toJSON = genericToJSON defaultOptions


isNone (PL D_NONE _) = True
isNone _             = False


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
    in if | p == "ROOT" -> ROOT
          | p == "NP"   -> NP
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



-- | chunk = chunktag, pos = postag, a = content
data PennTreeGen chunk pos a = PN chunk [PennTreeGen chunk pos a]
                             | PL pos a
                   deriving (Show, Functor, Foldable, Traversable)
                            
type PennTree = PennTreeGen Text Text Text

deriving instance (Eq chunk, Eq pos, Eq a) => Eq (PennTreeGen chunk pos a)
  

type Range = (Int,Int)

type PennTreeIdxG chunk pos a = PennTreeGen (Range,chunk) pos (Int,a)

type PennTreeIdx = PennTreeIdxG ChunkTag POSTag Text -- Text Text Text


trimap :: (c->c') -> (p->p') -> (a->a') -> PennTreeGen c p a -> PennTreeGen c' p' a'
trimap cf pf af (PN c xs) = PN (cf c) (map (trimap cf pf af) xs)
trimap cf pf af (PL p x) = PL (pf p) (af x)

mkIndexedTree :: PennTreeGen c p a -> PennTreeGen c p (Int,a)
mkIndexedTree tr = evalState (traverse tagidx tr) 0
  where tagidx x = get >>= \n -> put (n+1) >> return (n,x)

termRange :: PennTreeGen c p (Int,a) -> Range
termRange tr = let is = (map fst . toList) tr 
               in (minimum is,maximum is)

termRangeTree :: PennTreeGen c p (Int,a) -> PennTreeIdxG c p a 
termRangeTree tr@(PN c xs) = let is = (map fst . toList) tr 
                                 rng = (minimum is,maximum is)
                             in PN (rng,c) (map termRangeTree xs)
termRangeTree (PL p (n,x)) = PL p (n,x)


contain :: Int -> PennTreeGen c p (Int,a) -> [PennTreeGen c p  (Int,a)]
contain i y@(PN _ xs) = case (filter (not.null) . map (contain i)) xs of
                          [] -> []
                          ys:_ -> y:ys
contain i x@(PL _ (j,_)) | i == j = [x]
                         | otherwise = []


containR :: Range -> PennTreeIdxG c p a -> [PennTreeIdxG c p a]
containR r0 y@(PN (r,_) xs) | r0 == r = [y]
                            | otherwise = case (filter (not.null) . map (containR r0)) xs of
                                            [] -> []
                                            ys:_ -> y:ys
containR r0@(b,e) x@(PL _ (n,_)) = if b == n && e == n then [x] else []




getADTPennTree :: PennTree -> PennTreeGen ChunkTag POSTag Text
getADTPennTree = trimap identifyChunk identifyPOS id 



pruneOutNone :: Monoid m => PennTreeGen ChunkTag POSTag m -> PennTreeGen ChunkTag POSTag m
pruneOutNone (PN t xs) = let xs' = (filter (not . isNone) . map pruneOutNone) xs
                         in if null xs' then PL D_NONE mempty else PN t xs' 
pruneOutNone x = x 

mkPennTreeIdx :: PennTree -> PennTreeIdx
mkPennTreeIdx = termRangeTree . mkIndexedTree . getADTPennTree 
