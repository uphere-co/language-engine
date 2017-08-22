{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Type.PennTreebankII
( POSTag(..)
, ChunkTag(..)
, Trace(..)
, IOBPrefix(..)
, RelationTag(..)
, AnchorTag(..)
, TernaryLogic(..)
, isNone, isVerb, isNoun, isAdverb, isWHword
, linkIDChunk
, identifyPOS, identifyChunk, identifyTrace
, Bitree(..), LinkID(..)
, type PennTreeGen, type PennTree, type PennTreeIdxG, type PennTreeIdx, type PennTreeIdxA
, type Range
, Lemma(..)
, ANode(..)
, ALeaf(..)
, Annotation(..)
, type ANAtt, type ALAtt
, chunkTag, posTag, tokenWord, getTag, getRange
, termRange, termRangeTree, contain, containR
, mkIndexedTree, getADTPennTree, mkPennTreeIdx
, mkAnnotatable
) where

import           Control.Monad.Trans.State      (evalState,get,put)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Binary                    (Binary)
import           Data.Bitraversable
import           Data.Either                    (either)
import           Data.Foldable                  (toList)
import           Data.Maybe                     (catMaybes,isNothing,listToMaybe)
import           Data.Monoid                    ((<>))
import           Data.String                    (IsString)
import           Data.Text                      (Text)
import qualified Data.Text                 as T
import           Data.Text.Read                 (decimal)
import           GHC.Generics
--
import           Data.Attribute
import           Data.Bitree
import           Data.Range                     (Range)


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
            | M_HASH      -- ^ hash sign                         (original #)
            | D_LRB       -- ^ left parenthesis                  (original -LRB-)
            | D_RRB       -- ^ right parentheis                  (original -RRB-)
            | D_NONE      -- ^ none                              (original -NONE-)
              ----- new OntoNotes addendum
            | M_HYPH      -- ^ unbound hyphen                    (original HYPH)
            | AFX         -- ^ unbound affix
            | GW          -- ^ mistranscription (goes with)
            | XX          -- ^ uninterpretable material
              ---- not sure ye where it is from
            | NFP         -- ^ non-final punctuation
            deriving (Generic, Show, Eq, Ord, Enum, Bounded)

instance FromJSON POSTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON POSTag where
  toJSON = genericToJSON defaultOptions

instance Binary POSTag

isNone :: POSTag -> Bool
isNone D_NONE = True
isNone _      = False

isVerb :: POSTag -> Bool                     
isVerb VB  = True
isVerb VBZ = True
isVerb VBP = True
isVerb VBD = True
isVerb VBN = True
isVerb VBG = True
isVerb _   = False

data TernaryLogic = Yes | No | Unclear

isNoun :: POSTag -> TernaryLogic
isNoun CD   = Unclear
isNoun FW   = Unclear
isNoun NN   = Yes
isNoun NNS  = Yes
isNoun NNP  = Yes
isNoun NNPS = Yes
isNoun PRP  = Yes
isNoun SYM  = Unclear
isNoun WP   = Yes
isNoun _    = No


isWHword :: POSTag -> Bool
isWHword WDT      = True
isWHword WP       = True
isWHword WPDollar = True  
isWHword WRB      = True
isWHword _        = False


isAdverb :: POSTag -> Bool
isAdverb RB  = True
isAdverb RBR = True
isAdverb RBS = True
isAdverb RP  = True
isAdverb WRB = True
isAdverb _   = False

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
              -- from OntoNotes
              | NML       -- ^ mark nominal modifier
               
              deriving (Generic, Show,Eq,Ord,Enum,Bounded)

instance FromJSON ChunkTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ChunkTag where
  toJSON = genericToJSON defaultOptions

instance Binary ChunkTag


isWHphrase :: ChunkTag -> Bool
isWHphrase WHADJP = True
isWHphrase WHADVP = True
isWHphrase WHNP   = True
isWHphrase WHPP   = True
isWHphrase _      = False



data Trace = Tr_PRO   -- ^ overt subject, subject control and small clauses
           | Tr_STAR  -- ^ passive traces including reduced relative clauses
                      --   and raising constructions
           | Tr_T     -- ^ trace of A-movement, including parasitic gaps
           | Tr_NP    -- ^ arbitrary PRO, controlled PRO, and trace of A-movement
           | Tr_O     -- ^ null complementizer, including null wh-operator
           | Tr_U     -- ^ unit
           | Tr_QMARK -- ^ placeholder for ellipsed material
           | Tr_NOT   -- ^ anti-placeholder in template gapping
           | Tr_RNR   -- ^ pseudo-attach: right node raising
           | Tr_ICH   -- ^ pseudo-attach: interpret constituent here
           | Tr_EXP   -- ^ pseudo-attach: extraposition
           | Tr_PPA   -- ^ predictable ambiguous attachments
           deriving (Show,Eq,Ord,Enum,Bounded)


newtype LinkID = LinkID Int deriving (Show,Eq,Ord)


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

instance Binary RelationTag


data AnchorTag = A1       -- ^ anchor chunks that corresponds to P1
               | P1       -- ^ PNP that corresponds to A1
               deriving (Generic, Show,Eq,Ord,Enum)

instance FromJSON AnchorTag where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON AnchorTag where
  toJSON = genericToJSON defaultOptions

 
identifyPOS :: Text -> POSTag
identifyPOS t
  | t == "-LRB-"  = D_LRB
  | t == "-RRB-"  = D_RRB
  | t == "-NONE-" = D_NONE
  | otherwise =
    let p = T.takeWhile (\x -> x /= '-' && x /= '=') t
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
          | p == "#"    -> M_HASH
          -- new OntoNotes addendum
          | p == "HYPH" -> M_HYPH
          | p == "AFX"  -> AFX
          | p == "GW"   -> GW
          | p == "XX"   -> XX
          | p == "NFP"  -> NFP
          | otherwise   -> error ("invalid tag: " ++ T.unpack t)

identifyChunk :: Text -> ChunkTag
identifyChunk t =
    let p = T.takeWhile (\x -> x /= '-' && x /= '=') t
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
          -- OntoNotes
          | p == "NML"  -> NML
          | otherwise   -> error ("no such chunk tag : " ++ T.unpack t)


linkIDChunk :: Text -> [LinkID]
linkIDChunk t = 
  let ps = reverse (T.splitOn "-" t)
      f = either (const Nothing) (Just . LinkID . fst) . decimal
  in catMaybes . fst . break isNothing . map f $ ps


identifyTrace :: Text -> (Trace,Maybe LinkID)
identifyTrace t =
  let (p:ps) = T.splitOn "-" t
      tr = if | p == "*PRO*"  -> Tr_PRO
              | p == "*"      -> Tr_STAR
              | p == "*T*"    -> Tr_T
              | p == "(NP *)" -> Tr_NP
              | p == "0"      -> Tr_O
              | p == "*U*"    -> Tr_U
              | p == "*?*"    -> Tr_QMARK
              | p == "*NOT*"  -> Tr_NOT
              | p == "*RNR*"  -> Tr_RNR
              | p == "*ICH*"  -> Tr_ICH
              | p == "*EXP*"  -> Tr_EXP
              | p == "*PPA*"  -> Tr_PPA
              | otherwise   -> error ("no such trace tag : " ++ T.unpack t)
      mn = either (const Nothing) (Just . LinkID . fst) . decimal =<< listToMaybe ps
  in (tr,mn)


-- | chunk = chunktag, token = token in node. 
--   typically token will be (pos = postag, a = content)
type PennTreeGen chunk token = Bitree chunk token
    
type PennTree = Bitree Text (Text,Text)


newtype Lemma = Lemma { unLemma :: Text }
              deriving (Show,Eq,Ord,FromJSON,ToJSON,IsString)

type PennTreeIdxG chunk token = PennTreeGen (Range,chunk) (Int,token)

type PennTreeIdx = PennTreeIdxG ChunkTag (POSTag,Text)

data ANode annot = ANode ChunkTag annot

data ALeaf annot = ALeaf (POSTag,Text) annot

type ANAtt lst = ANode (AttribList lst)

type ALAtt lst = ALeaf (AttribList lst)

class Annotation f where
  getAnnot :: f a -> a

instance Annotation ANode where
  getAnnot (ANode _ a) = a

instance Annotation ALeaf where
  getAnnot (ALeaf _ a) = a
  

chunkTag :: ANode a -> ChunkTag
chunkTag (ANode c _) = c

posTag :: ALeaf a -> POSTag
posTag (ALeaf (p,_) _) = p

tokenWord :: ALeaf a -> Text
tokenWord (ALeaf (_,t) _) = t

type PennTreeIdxA = PennTreeIdxG (ANode (AttribList '[])) (ALeaf (AttribList '[]))


getTag :: PennTreeIdxG c (p,a) -> Either c p
getTag (PN (_,c) _)   = Left c
getTag (PL (_,(p,_))) = Right p

getRange :: PennTreeIdxG c t -> Range
getRange (PN (r,_) _) = r
getRange (PL (n,_)) = (n,n)


mkIndexedTree :: PennTreeGen c t -> PennTreeGen c (Int,t)
mkIndexedTree tr = evalState (traverse tagidx tr) 0
  where tagidx x = get >>= \n -> put (n+1) >> return (n,x)

termRange :: PennTreeGen c (Int,t) -> Range
termRange tr = let is = (map fst . toList) tr 
               in (minimum is,maximum is)

termRangeTree :: PennTreeGen c (Int,t) -> PennTreeIdxG c t
termRangeTree tr@(PN c xs) = let is = (map fst . toList) tr 
                                 rng = (minimum is,maximum is)
                             in PN (rng,c) (map termRangeTree xs)
termRangeTree (PL (n,t)) = PL (n,t)


contain :: Int -> PennTreeGen c (Int,t) -> [PennTreeGen c (Int,t)]
contain i y@(PN _ xs) = case (filter (not.null) . map (contain i)) xs of
                          [] -> []
                          ys:_ -> y:ys
contain i x@(PL (j,_)) | i == j = [x]
                       | otherwise = []


containR :: Range -> PennTreeIdxG c t -> [PennTreeIdxG c t]
containR r0 y@(PN (r,_) xs) | r0 == r = [y]
                            | otherwise = case (filter (not.null) . map (containR r0)) xs of
                                            [] -> []
                                            ys:_ -> y:ys
containR r0@(b,e) x@(PL (n,_)) = if b == n && e == n then [x] else []


getADTPennTree :: PennTree -> PennTreeGen ChunkTag (POSTag, Text)
getADTPennTree = bimap identifyChunk (first identifyPOS)


{- 
pruneOutNone :: Monoid m => PennTreeGen ChunkTag POSTag m -> PennTreeGen ChunkTag POSTag m
pruneOutNone (PN t xs) = let xs' = (filter (not . isNone) . map pruneOutNone) xs
                         in if null xs' then PL D_NONE mempty else PN t xs' 
pruneOutNone x = x 
-}

mkPennTreeIdx :: PennTree -> PennTreeIdx
mkPennTreeIdx = termRangeTree . mkIndexedTree . getADTPennTree 

mkAnnotatable :: PennTreeIdx -> PennTreeIdxA
mkAnnotatable = bimap (\(i,x) -> (i,ANode x anil)) (\(j,y)-> (j,ALeaf y anil))
