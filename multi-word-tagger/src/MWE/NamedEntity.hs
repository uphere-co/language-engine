{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MWE.NamedEntity where

import           Control.Lens               ((^.),(^..),_1,_2)
import           Control.Monad.State
import           Data.List                  (find,intersperse)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text             as T
--
import           NLP.Type.CoreNLP


getReplacedTextWithNewWikiEL :: [Sentence] -> [(Int,Int)] -> [(Text,[(Int,Int)])]
getReplacedTextWithNewWikiEL sents constraint = do
  let tokenss = catMaybes <$> sents ^.. traverse . sentenceToken
      cinchar = catMaybes $ map (\c -> convertConstraintInCharIdx c tokenss) constraint

  taggedTokenss <- flip evalStateT constraint $ do
    forM tokenss $ \tokens -> do
      forM tokens $ \t -> do
        s <- get
        if (not $ null s)
          then do
          if ((t ^. token_tok_idx_range) `contained` (head s))
            then do
            if ((t ^. token_tok_idx_range ^. _2) == (snd (head s)))
              then do
              modify' $ (\xs -> drop 1 xs)
              return (t,2)
              else return (t,1)
            else return (t,0)
          else return (t,0)

  let changeToken :: Token -> Token -> Int -> Int -> Text
      changeToken t1 t2 n1 n2 = case n1 of
        0 -> fillGap t1 t2 n1 n2 " "
        1 -> fillGap t1 t2 n1 n2 "-"
        2 -> fillGap t1 t2 n1 n2 " "
        _ -> error "changeToken in getReplacedTextWithNewWikEL"
      gapLength t1 t2 = (t2 ^. token_char_idx_range ^. _1) - (t1 ^. token_char_idx_range ^. _2)
      fillGap t1 t2 n1 _ char  = if (n1 == 1 || n1 == 2)
                                 then T.replace "&" "AND" $ T.replace "." "-PERIOD" $ (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char))
                                 else (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char))
      result' = (map (\xs -> (map (\((t1,n1),(t2,n2)) -> changeToken t1 t2 n1 n2) (zip xs (drop 1 xs))) ++ [last xs ^. _1 ^. token_text] ) taggedTokenss)
      result = T.intercalate "" $ concat $ intersperse ["\n"] result'
  newwikiel <- mkNewWikiEL cinchar tokenss
  return (result,newwikiel)
  

contained :: (Ord a) => (a,a) -> (a,a) -> Bool
contained (a,b) (c,d) = if (a >= c && b <=d) then True else False


convertConstraintInCharIdx :: (Int,Int) -> [[Token]] -> Maybe (Int,Int)
convertConstraintInCharIdx (b,e) tokss =
  let tb' = find (\t -> (t ^. token_tok_idx_range ^. _1) == b) (concat tokss)
      te' = find (\t -> (t ^. token_tok_idx_range ^. _2) == e) (concat tokss)
  in case (tb',te') of
    (Just tb, Just te) -> Just $ (tb ^. token_char_idx_range ^. _1, te ^. token_char_idx_range ^. _2)
    _                  -> Nothing


findNETokens :: (Int,Int) -> [[Token]] -> [Token]
findNETokens con tokss = filter (\t -> (t ^. token_char_idx_range) `contained` con) (concat tokss)


mkNewWikiEL :: [(Int,Int)] -> [[Token]] -> [[(Int,Int)]]
mkNewWikiEL cons tokss = do
  result <- flip evalStateT (0 :: Int) $ do
    forM cons $ \(a,b) -> do
      s <- get
      let ff (x,y) = ((*) 6 $ length $ filter (\t -> '.' `elem` (T.unpack (t ^. token_text))) $ findNETokens (x,y) tokss ) +
                     ((*) 2 $ length $ filter (\t -> '&' `elem` (T.unpack (t ^. token_text))) $ findNETokens (x,y) tokss )
      modify' $ (\s' -> s' + (ff (a,b)))
      return (a + s, b + s + ff (a,b))
  return result
