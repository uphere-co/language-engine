{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module WordNet.Parser.Lexicographer where

import           Control.Applicative             (many,optional,pure
                                                 ,(<$>),(<*>),(<*),(*>),(<|>))
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


p_letters = takeWhile1 (\c -> inClass "a-zA-Z" c || c == '-')

p_lexfile = foldl1' (<|>) (map (\(x,y) -> string x *> pure y) lexicographerFileTable)


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


p_wordpointer = do
  char '['
  skipSpace
  w <- p_word
  ps <- many1 (skipSpace *> p_pointer)
  skipSpace
  char ']'
  return (w,ps)

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
  wps <- many (skipSpace *> p_wordpointer)
  ws <- many (skipSpace *> p_word)
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  char '('
  gloss' <- manyTill anyChar (string ") }")
  return (Synset wps ws ps [] (T.pack gloss'))

p_skipEmptyLine = skipWhile (\c -> c == ' ' || c == '\n')

p_synset_verb = undefined

p_synset_adjective = undefined

p_synset_adverb = undefined


  
