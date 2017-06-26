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
import           Data.Char                       (digitToInt,isAlpha,isDigit)
import           Data.List                       (foldl1')
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                  as T
-- import qualified Data.Text.Read             as TR
--
import           NLP.Type.WordNet
import           WordNet.Type
import           WordNet.Type.Lexicographer
--
import qualified Data.Text.IO as TIO

pointerSymbol_table :: [ (PointerSymbol, Text) ]
pointerSymbol_table 
  = [ (Antonym                  , "!" )
    , (Instance_Hypernym        , "@i")  -- ordering is important
    , (Hypernym                 , "@" )
    , (Instance_Hyponym         , "~i")  -- ordering is important
    , (Hyponym                  , "~" )
    , (Member_Holohym           , "#m")
    , (Substance_Holonym        , "#s")
    , (Part_Holonym             , "#p")
    , (Member_Meronym           , "%m")
    , (Substance_Meronym        , "%s")
    , (Part_Meronym             , "%p")
    , (Attribute                , "=" )
    , (DerivationallyRelatedForm, "+" )
    , (Entailment               , "*" )
    , (Cause                    , ">" )
    , (AlsoSee                  , "^" )
    , (VerbGroup                , "$" )
    , (SimilarTo                , "&" )
    , (ParticipleOfVerb         , "<" )
    , (BackSlash                , "\\")  -- backslash is pertainym for adjective, derived from adjective for adverb
    , (DomainOfSynset_TOPIC     , ";c")
    , (MemberOfThisDomain_TOPIC , "-c")
    , (DomainOfSynset_REGION    , ";r")
    , (MemberOfThisDomain_REGION, "-r")
    , (DomainOfSynset_USAGE     , ";u")
    , (MemberOfThisDomain_USAGE , "-u")
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

p_word_marker_lexid = do
  ws <- do ws' <- many (p_nonlasttoken <* char '_')
           w <- p_token
           return (ws'++[w])
  mk <- optional ( (string "(p)"  >> return Marker_P) <|>
                   (string "(a)"  >> return Marker_A) <|>
                   (string "(ip)" >> return Marker_IP)
                 )
  md :: Maybe Int <- optional (read <$> many1 digit)
  return (ws,mk,md)

p_word = do
  (ws,mk,md) <- p_word_marker_lexid
  char ','
  c <- peekChar'
  guard (c == ' ' || isAlpha c ) -- this is due to verb.motion:body-surf
  return (SSWord ws mk md)

p_pointer :: Parser SSPointer
p_pointer = do
  lexfile <- optional (p_lexfile <* char ':')
  skipSpace
  (ws,md) <- p_word_lexid
  msatellite <- optional (char '^' *> p_word_lexid)
  char ','
  s <-p_pointer_symbol
  return (SSPointer lexfile ws md msatellite s)

p_frames = do
  string "frames:"
  skipSpace 
  xs <- decimal `sepBy1` (char ',' >> skipSpace)
  -- TR.decimal
  return xs

p_wordpointer isVerb = do
  char '['
  skipSpace
  w <- p_word
  -- skipSpace
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  fs <- if isVerb 
          then fromMaybe [] <$> optional (p_frames <* skipSpace)
          else pure []
  char ']'
  return (w,ps,fs)

  
p_pointer_symbol :: Parser PointerSymbol
p_pointer_symbol = foldl1' (<|>) (map (\(x,y)-> string y *> return x) pointerSymbol_table)


p_synset_noun_adv = do
  char '{'
  wps <- many1 (skipSpace *> (fmap Left p_word <|> fmap Right (p_wordpointer False)))
  skipSpace  
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  char '('
  gloss' <- manyTill anyChar (char ')' >> skipSpace >> char '}')
  manyTill anyChar endOfLine
  return (Synset wps ps [] (T.pack gloss'))

p_synset_verb = do
  char '{'
  wps <- many1 (skipSpace *> (fmap Left p_word <|> fmap Right (p_wordpointer True)))
  skipSpace
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  fs <- p_frames
  skipSpace
  -- this is a workaround because of verb.emotion:attract
  ps' <- many (skipSpace *> p_pointer) 
  skipSpace
  fs' <- optional p_frames 
  skipSpace
  -- up to here  
  char '('
  gloss' <- manyTill anyChar (char ')' >> skipSpace >> char '}')
  manyTill anyChar endOfLine
  return (Synset wps ps fs (T.pack gloss'))

  

p_synset_adjective = do
  char '{'
  wps <- many1 (skipSpace *> (fmap Left p_word <|> fmap Right (p_wordpointer False)))
  skipSpace  
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  char '('
  gloss' <- manyTill anyChar (char ')' >> skipSpace >> char '}')
  manyTill anyChar endOfLine
  return (Synset wps ps [] (T.pack gloss'))


p_synset :: SSType -> Parser (Maybe Synset)
p_synset t = (Just <$> p) <|> (p_comment *> return Nothing) <|> (p_empty *> return Nothing)
  where p = case t of
              Noun      -> p_synset_noun_adv
              Verb      -> p_synset_verb
              Adjective -> p_synset_adjective
              Adverb    -> p_synset_noun_adv
  
