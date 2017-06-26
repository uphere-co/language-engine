{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module WordNet.Parser.Lexicographer where

import           Control.Applicative             (many,optional,pure
                                                 ,(<$>),(<*>),(<*),(*>),(<|>))
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char                       (digitToInt,isDigit)
import           Data.List                       (foldl1')
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           NLP.Type.WordNet
import           WordNet.Type
import           WordNet.Type.Lexicographer
--
import qualified Data.Text.IO as TIO

pointerSymbol_table :: SSSType a -> [ (PointerSymbol a, Text) ]
pointerSymbol_table SNoun
  = [ (PSN_Antonym                  , "!" )
    , (PSN_Instance_Hypernym        , "@i")  -- ordering is important
    , (PSN_Hypernym                 , "@" )
    , (PSN_Instance_Hyponym         , "~i")  -- ordering is important
    , (PSN_Hyponym                  , "~" )
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
    , (PSN_Pertainym                , "\\")
    ]
pointerSymbol_table SVerb
  = [ (PSV_Antonym                  , "!" )
    , (PSV_Hypernym                 , "@" )
    , (PSV_Hyponym                  , "~" )
    , (PSV_Entailment               , "*" )
    , (PSV_Cause                    , ">" )
    , (PSV_AlsoSee                  , "^" )
    , (PSV_VerbGroup                , "$" )
    , (PSV_DerivationallyRelatedFrom, "+" )
    , (PSV_DomainOfSynset_TOPIC     , ";c")
    , (PSV_DomainOfSynset_REGION    , ";r")
    , (PSV_DomainOfSynset_USAGE     , ";u")
    ]
pointerSymbol_table SAdjective
  = [ (PSJ_Antonym                  , "!" )
    , (PSJ_SimilarTo                , "&" )
    , (PSJ_ParticipleOfVerb         , "<" )
    , (PSJ_Pertainym                , "\\")
    , (PSJ_Attribute                , "=" )
    , (PSJ_AlsoSee                  , "^" )
    , (PSJ_DomainOfSynset_TOPIC     , ";c")
    , (PSJ_DomainOfSynset_REGION    , ";r")
    , (PSJ_DomainOfSynset_USAGE     , ";u")
    ]
pointerSymbol_table SAdverb
  = [ (PSR_Antonym                  , "!" )
    , (PSR_DerviedFromAdjective     , "\\")
    , (PSR_DomainOfSynset_TOPIC     , ";c")
    , (PSR_DomainOfSynset_REGION    , ";r")
    , (PSR_DomainOfSynset_USAGE     , ";u")
    ]

p_comment = char '(' >> manyTill anyChar endOfLine

p_empty = skipWhile (\c -> c == ' ' || c == '\t') >> endOfLine

p_skipEmptyLine = skipWhile (\c -> c == ' ' || c == '\n')

p_escapednumber = takeWhile1 isDigit <* char '"'

isLetter c = inClass "a-zA-Z" c || c `elem` ['-','.','\'','/']

p_letters = takeWhile1 isLetter

p_nonlasttoken = T.concat <$> many (p_letters <|> p_escapednumber <|> takeWhile1 isDigit)

p_token = do
  xs <- many $ do txt <- p_escapednumber <|>
                         (do num <- Data.Attoparsec.Text.takeWhile1 isDigit
                             txt1 <- p_letters
                             return (num <> txt1)) <|>
                         p_letters
                  return txt
  return $ T.concat xs

p_lexfile = foldl1' (<|>) (map (\(x,y) -> string x *> pure y) lexicographerFileTable)

p_word_lexid = do
  ws <- do ws' <- many (p_nonlasttoken <* char '_')
           w <- p_token
           return (ws'++[w]) 
  md :: Maybe Int <- optional (read <$> many1 digit)
  return (ws,md)

p_word = do
  (ws,md) <- p_word_lexid
  char ','
  char ' '
  return (SSWord ws Nothing md)

p_pointer :: SSType -> Parser SSPointer
p_pointer defsstyp = do
  lexfile <- optional (p_lexfile <* char ':')
  skipSpace
  (ws,md) <- p_word_lexid
  msatellite <- optional (char '^' *> p_word_lexid)
  char ','
  s <-p_pointer_symbol defsstyp
  return (SSPointer lexfile ws md msatellite s)


p_wordpointer defsstyp = do
  char '['
  skipSpace
  w <- p_word
  ps <- many1 (skipSpace *> p_pointer defsstyp)
  skipSpace
  char ']'
  char ' '
  return (w,ps)


p_pointer_symbol :: SSType -> Parser PointerSymbolAll
p_pointer_symbol Noun      = PSNoun      <$> p_pointer_symbol_gen SNoun
p_pointer_symbol Verb      = PSVerb      <$> p_pointer_symbol_gen SVerb
p_pointer_symbol Adjective = PSAdjective <$> p_pointer_symbol_gen SAdjective
p_pointer_symbol Adverb    = PSAdverb    <$> p_pointer_symbol_gen SAdverb

  
p_pointer_symbol_gen :: SSSType a -> Parser (PointerSymbol a)
p_pointer_symbol_gen = foldl1' (<|>)
                       . map (\(x,y)-> string y *> return x)
                       . pointerSymbol_table

p_synset_noun = do
  char '{'
  wps <- many1 (skipSpace *> (fmap Left p_word <|> fmap Right (p_wordpointer Noun)))
  -- ws <- many (skipSpace *> p_word)
  ps <- many (skipSpace *> (p_pointer Noun))
  skipSpace
  char '('
  gloss' <- manyTill anyChar (char ')' >> skipSpace >> char '}')
  -- skipWhile (== ' ')
  -- optional p_comment
  manyTill anyChar endOfLine
  return (Synset wps ps [] (T.pack gloss'))



p_synset_verb = undefined

p_synset_adjective = undefined

p_synset_adverb = undefined


  

p_synset :: SSType -> Parser (Maybe Synset)
p_synset Noun      = (Just <$> p_synset_noun) <|>
                      (p_comment *> return Nothing) <|>
                      (p_empty *> return Nothing)
p_synset Verb      = p_synset_verb
p_synset Adjective = p_synset_adjective
p_synset Adverb    = p_synset_adverb

