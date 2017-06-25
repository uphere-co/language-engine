{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module WordNet.Parser.Lexicographer where

import           Data.Attoparsec.Text
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           WordNet.Type.Lexicographer
--
import qualified Data.Text.IO as TIO

p_synset :: SSSType typ -> Parser (Synset typ)
p_synset SNoun      = p_synset_noun
p_synset SVerb      = p_synset_verb
p_synset SAdjective = p_synset_adjective
p_synset SAdverb    = p_synset_adverb


p = takeWhile1 (inClass "0-9a-zA-Z_:.") <* skipSpace

  
p_synset_noun = do
  char '{'
  skipSpace
  txts <- many1 (p <* char ',' <* skipSpace) <* char '@'
  let xs = map (\x->SSWord [x] Nothing Nothing) txts
  return (Synset xs [] [] Nothing) 

p_synset_verb = undefined

p_synset_adjective = undefined

p_synset_adverb = undefined


main = do
  let fp = "/scratch/wavewave/wordnet/WordNet-3.1/dict/dbfiles/noun.animal"
  txt <- TIO.readFile fp
  let testtxt = head (drop 5 (T.lines txt))
  print $ parseOnly (p_synset SNoun) testtxt 
  
