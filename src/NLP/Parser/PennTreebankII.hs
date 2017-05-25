module NLP.Parser.PennTreebankII where

import           Data.Text                   (Text)
import           Control.Applicative
import qualified Data.Attoparsec.Text as A
--
import           NLP.Type.PennTreebankII
--
import Debug.Trace

isNone (PL D_NONE _) = True
isNone _             = False

{- 
xformPennTree :: PennTreeGen Text Text Text -> PennTreeGen ChunkTag POSTag Text
xformPennTree (PN tn xs) = PN (identifyChunk tn) (map xformPennTree xs)
xformPennTree (PL tl x)  = PL (identifyPOS tl) x

pruneOutNone :: Monoid m => PennTreeGen ChunkTag POSTag m -> PennTreeGen ChunkTag POSTag m
pruneOutNone (PN t xs) = let xs' = (filter (not . isNone) . map pruneOutNone) xs
                         in if null xs' then PL D_NONE mempty else PN t xs' 
pruneOutNone x = x 
-}


penntree :: A.Parser (PennTreeGen Text Text Text)
penntree = do
  oparen
  A.skipSpace
  n <- pnode
  A.skipSpace
  cparen
  return n

pnode :: A.Parser (PennTreeGen Text Text Text)
pnode =
    (do oparen
        t <- tag
        A.skipSpace
        s <- A.many1 (pnode <* A.skipSpace)
        A.skipWhile (/= ')')
        cparen 
        return (PN t s))
    <|> 
    (do oparen
        t <- tag
        A.skipSpace
        c <- (A.takeWhile1 (A.notInClass " ()"))
        A.skipWhile (/= ')')
        cparen
        return (PL t c))


oparen :: A.Parser Char
oparen = A.char '('

cparen :: A.Parser Char
cparen = A.char ')'

tag :: A.Parser Text
tag = A.takeWhile (`elem` ([ 'A'..'Z' ] ++ ['0'..'9'] ++ ".,-:'`$"))


{- 
bintree :: A.Parser (BinTree Text)
bintree = 
  (do oparen
      A.skipSpace
      -- s <- A.many1 (penntree <* A.skipSpace)
      n1 <- bintree
      A.skipSpace
      n2 <- bintree
      A.skipWhile (/= ')')
      cparen 
      return (BinNode n1 n2))
  <|> binleaf

binleaf :: A.Parser (BinTree Text)
binleaf = do    
  s <- A.takeWhile1 (not . (`elem` ['(',')',' ']))
  return (BinLeaf s)
-}

