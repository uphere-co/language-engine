{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Data where

import           Data.List.Split     (chunksOf)
import           Data.Text           (Text)
import qualified Data.Text    as T
--
import           WordNet.Parser.Common
import           WordNet.Type
import           WordNet.Type.Lexicographer (LexicographerID(..))
import           WordNet.Type.POS


readSSType :: Char -> Maybe SSType
readSSType 'n' = Just Noun
readSSType 'v' = Just Verb
readSSType 'a' = Just Adjective
readSSType 's' = Just AdjectiveSatellite
readSSType 'r' = Just Adverb
readSSType _   = Nothing

parseLexItem :: [Text] -> Maybe LexItem
parseLexItem (x:y:[]) = LI <$> pure x <*> (LexicographerID <$> readDecimal y)
parseLexItem _        = Nothing

parseFrame :: Int -> [Text] -> Maybe ([Frame],[Text])
parseFrame n txts = do
  let (frametxts,r) = splitAt n (chunksOf 3 txts)
      p_frame (_:x:y:[]) = do f_num <- readDecimal x
                              w_num <- readDecimal y
                              return (Frame f_num w_num)
      p_frame _ = Nothing
  fs <- mapM p_frame frametxts
  return (fs,concat r)


parseData :: Bool -> Text -> Maybe DataItem
parseData isVerb = worker . T.words
  where
    worker (o':num':typ':cnt':rem0) = do
      o <- readDecimal o'
      num <- readDecimal num'
      typ <- if T.null typ' then Nothing else readSSType (T.head typ')
      cnt <- readDecimal cnt'
      let (wlexstr,rem') = splitAt cnt (chunksOf 2 rem0)
          pcnt':rem1 = concat rem'
      
      wordlexids <- mapM parseLexItem wlexstr
      pcnt <- readDecimal pcnt'
      let (ptrstr,rem2) = splitAt pcnt (chunksOf 4 rem1)
          p_ptr (z1:z2:z3:z4:[]) = do
            off <- readDecimal z2
            let st = T.splitAt 2 z4
            return (Pointer z1 off z3 st)
          p_ptr _ = Nothing
          rem2'@(rem20:rem2s) = concat rem2
      ptrs <- mapM p_ptr ptrstr
      (fs,rem3) <- if isVerb
                     then do
                       n <- readDecimal rem20
                       parseFrame n rem2s
                     else return ([],rem2')
      let _:comments = rem3 
      return (DataItem o num typ wordlexids ptrs fs (T.intercalate " " comments))
    worker _ = Nothing
  
