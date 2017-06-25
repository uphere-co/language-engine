{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module WordNet.Parser.Lexicographer where

import           Control.Applicative             (optional,pure,(<$>),(<*>),(<*),(*>),(<|>))
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char                       (digitToInt)
import           Data.List                       (foldl1')
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           NLP.Type.WordNet
import           WordNet.Type
import           WordNet.Type.Lexicographer
--
import qualified Data.Text.IO as TIO

p_synset :: SSSType typ -> Parser (Synset typ)
p_synset SNoun      = p_synset_noun
p_synset SVerb      = p_synset_verb
p_synset SAdjective = p_synset_adjective
p_synset SAdverb    = p_synset_adverb


p_letters = takeWhile1 (inClass "a-zA-Z")

p_lexfile = foldl1' (<|>) (map (\(x,y) -> string x *> pure y) lexicographerFileTable)

{- 
  (string "adj.all"    *> pure AdjAll) <|>
            (string "adj.pert"   *> pure AdjPert) <|>
            (string "noun.group" *> pure NounGroup)
-}

--                         "_:."

p_word = do
  ws <- p_letters `sepBy1` char '_'
  md <- optional (digitToInt <$> digit)
  char ','
  return (SSWord ws Nothing md)

p_pointer :: Parser (SSPointer 'Noun)
p_pointer = do
  lexfile <- optional (p_lexfile <* char ':')
  ws <- p_letters `sepBy1` char '_'
  md <- optional (digitToInt <$> digit)
  char ','
  s <-p_pointer_symbol_noun
  return (SSPointer lexfile ws md s)


pointerSymbol_noun_table :: [ (PointerSymbol 'Noun, Text) ]
pointerSymbol_noun_table = [ (PSN_Antonym                  , "!" )
                           , (PSN_Hypernym                 , "@" )
                           , (PSN_Instance_Hypernym        , "@i")
                           , (PSN_Hyponym                  , "~" )
                           , (PSN_Instance_Hyponym         , "~i")
                           , (PSN_Member_Holohym           , "#m")
                           , (PSN_Substance_Holonym        , "#s")
                           , (PSN_Part_Holonym             , "#p")
                           , (PSN_Member_Meronym           , "%m")
                           , (PSN_Substance_Meronym        , "%s")
                           , (PSN_Part_Meronym             , "%p")
                           , (PSN_Attribute                , "=" )
                           , (PSN_DerivationallyRelatedForm, "+" )
                           , (PSN_DomainOfSynset_TOPIC     , ";c")
                           , (PSN_MemberOfThisDomain_TOPIC , "-c")
                           , (PSN_DomainOfSynset_REGION    , ";r")
                           , (PSN_MemberOfThisDomain_REGION, "-r")
                           , (PSN_DomainOfSynset_USAGE     , ";u")
                           , (PSN_MemberOfThisDomain_USAGE , "-u")
                           ]
  
p_pointer_symbol_noun :: Parser (PointerSymbol 'Noun)
p_pointer_symbol_noun
  = foldl1' (<|>) (map (\(x,y)-> string y *> return x) pointerSymbol_noun_table)

  
p_synset_noun = do
  char '{'
  ws <- many1 (skipSpace *> p_word)
  skipSpace
  ps <- many1 p_pointer
  skipSpace
  char '('
  gloss <- takeTill (== ')')
  char ')'
  skipSpace
  char '}'
  -- char ','
  -- skipSpace *> char '@' -}
  --  many1 (p <* char ',' <* skipSpace) <* char '@'
  -- let xs = map (\ws ->SSWord ws Nothing Nothing) txts
  return (Synset ws ps [] gloss) 

p_synset_verb = undefined

p_synset_adjective = undefined

p_synset_adverb = undefined


main = do
  let fp = "/scratch/wavewave/wordnet/WordNet-3.1/dict/dbfiles/noun.animal"
  txt <- TIO.readFile fp
  let testtxt = head (drop 5 (T.lines txt))
  print $ parse (p_synset SNoun) testtxt 
  
