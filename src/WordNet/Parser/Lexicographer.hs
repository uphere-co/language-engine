{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module WordNet.Parser.Lexicographer where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char                       (isAlpha,isDigit)
import           Data.List                       (foldl1')
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS


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


p_comment :: Parser ()
p_comment = char '(' >> void (manyTill anyChar endOfLine)


p_empty :: Parser ()
p_empty = skipWhile (\c -> c == ' ' || c == '\t') >> endOfLine


p_skipEmptyLine :: Parser ()
p_skipEmptyLine = skipWhile (\c -> c == ' ' || c == '\n')


p_escapednumber :: Parser Text
p_escapednumber = takeWhile1 isDigit <* char '"'


isLetter :: Char -> Bool
isLetter c = inClass "a-zA-Z" c || c `elem` ['-','.','\'','/']


p_letters :: Parser Text
p_letters = takeWhile1 isLetter


p_nonlasttoken :: Parser Text
p_nonlasttoken = T.concat <$> many (p_letters <|> p_escapednumber <|> takeWhile1 isDigit)


p_token :: Parser Text
p_token = do
  xs <- many $ do txt <- p_escapednumber <|>
                         (do num <- Data.Attoparsec.Text.takeWhile1 isDigit
                             txt1 <- p_letters
                             return (num <> txt1)) <|>
                         p_letters
                  return txt
  return $ T.concat xs


p_lexfile :: Parser (Either Text LexicographerFile)
p_lexfile = foldl1' (<|>) (map (\(x,y) -> string x *> pure y) lexicographerFileTable)

{- 
p_word_lexid :: Parser ([Text],Maybe Int)
p_word_lexid = do
  ws <- do ws' <- many (p_nonlasttoken <* char '_')
           w <- p_token
           return (ws'++[w])
  md :: Maybe Int <- optional (read <$> many1 digit)
  return (ws,md)
-}

p_word_lexid_marker :: Parser ([Text],Maybe Int,Maybe Marker)
p_word_lexid_marker = do
  ws <- do ws' <- many (p_nonlasttoken <* char '_')
           w <- p_token
           return (ws'++[w])
  md :: Maybe Int <- optional (read <$> many1 digit)
  mk <- optional ( (string "(p)"  >> return Marker_P) <|>
                   (string "(a)"  >> return Marker_A) <|>
                   (string "(ip)" >> return Marker_IP)
                 )
  return (ws,md,mk)


p_word :: Parser SSWord
p_word = do
  (ws,md,mk) <- p_word_lexid_marker
  char ','
  c <- peekChar'
  guard (c == ' ' || c == '[' || isAlpha c ) -- this is due to verb.motion:body-surf
  return (SSWord ws md mk)


p_pointer :: Parser SSPointer
p_pointer = do
  lexfile <- optional (p_lexfile <* char ':')
  skipSpace
  (ws,md,mk) <- p_word_lexid_marker
  msatellite <- optional (char '^' *> fmap (\(x,y,z) -> SSWord x y z) p_word_lexid_marker)
  char ','
  s <-p_pointer_symbol
  return (SSPointer lexfile (SSWord ws md mk) msatellite s)


p_frames :: Parser [Int]
p_frames = do
  string "frames:"
  skipSpace 
  xs <- decimal `sepBy1` (char ',' >> skipSpace)
  -- TR.decimal
  return xs


p_wordpointer :: Bool -> Parser (SSWord,[SSPointer],[Int])
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


p_synset_gen :: Bool -> Parser Synset
p_synset_gen isVerb = do
  char '{'
  wps <- many1 (skipSpace *> (fmap Left p_word <|> fmap Right (p_wordpointer True)))
  skipSpace
  ps <- many (skipSpace *> p_pointer)
  skipSpace
  (fs,ps',fs') <- if isVerb
    then do
      fs <- p_frames
      skipSpace
      -- this is a workaround because of verb.emotion:attract
      ps' <- many (skipSpace *> p_pointer) 
      skipSpace
      fs' <- optional p_frames 
      skipSpace
      -- up to here      
      return (fs,ps',fs')
    else return ([],[],Nothing)
  char '('
  gloss' <- manyTill anyChar (char ')' >> skipSpace >> char '}')
  -- manyTill anyChar endOfLine
  return (Synset wps (ps++ps') (fs++fromMaybe [] fs') (T.pack gloss'))


p_synset :: SSType -> Parser (Maybe Synset)
p_synset t = (Just <$> (p <* manyTill anyChar endOfLine)) <|> (p_comment *> return Nothing) <|> (p_empty *> return Nothing)
  where p = case t of
              Noun      -> p_synset_gen False -- p_synset_noun_adv
              Verb      -> p_synset_gen True  -- p_synset_verb
              Adjective -> p_synset_gen False -- p_synset_adjective
              Adverb    -> p_synset_gen False -- p_synset_noun_adv
              x         -> error ("p_synset: I do not know how to deal with " ++ show x)


p_splitter :: Parser ()
p_splitter = void (string "----")


p_head_satellites :: Parser (Synset,[Synset])
p_head_satellites = 
  (,) <$> (p_synset_gen False <* skipSpace)
      <*> (p_synset_gen False `sepBy` skipSpace)

  
p_synset_adj_cluster :: Parser (Maybe (Either Synset SynsetCluster))
p_synset_adj_cluster = (Just . Right <$> p1) <|> (Just . Left <$> p2) <|> (p_comment *> return Nothing) <|> (p_empty *> return Nothing)
  where p1 = do char '['
                skipSpace
                r <- SynsetCluster <$> (p_head_satellites `sepBy1` (skipSpace >> p_splitter >> skipSpace))
                skipSpace
                char ']'
                return r
        p2 = p_synset_gen False

