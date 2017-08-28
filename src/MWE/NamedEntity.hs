{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MWE.NamedEntity where


import           Control.Lens
import           Control.Monad.State
import           Data.List                  (find,intersperse)
import           Data.Maybe                 (catMaybes,isJust)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
--
import           NLP.Type.CoreNLP


preProcessing sents constraint = do
  let tokenss = catMaybes <$> sents ^.. traverse . sentenceToken
  let constraint' = catMaybes $ map (\c -> convertConstraintInCharIdx c tokenss) constraint

  a <- flip evalStateT constraint $ do
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

  let f t1 t2 n1 n2 = case n1 of
        0 -> fillGap t1 t2 n1 n2 " "
        1 -> fillGap t1 t2 n1 n2 "-"
        2 -> fillGap t1 t2 n1 n2 " "
      gapLength t1 t2 = (t2 ^. token_char_idx_range ^. _1) - (t1 ^. token_char_idx_range ^. _2)
      fillGap t1 t2 n1 n2 char = if (n1 == 1 || n1 == 2)
                                 then T.replace "&" "AND" $ T.replace "." "-PERIOD" $ (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char))
                                 else (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char))
      result' = (map (\xs -> (map (\((t1,n1),(t2,n2)) -> f t1 t2 n1 n2) (zip xs (drop 1 xs))) ++ [last xs ^. _1 ^. token_text] ) a)
      result = T.intercalate "" $ concat $ intersperse ["\n"] result'

  print tokenss
  TIO.putStrLn result
  print constraint'
  mkNewWikiEL constraint' tokenss >>= print




contained :: (Ord a) => (a,a) -> (a,a) -> Bool
contained (a,b) (c,d) = if (a >= c && b <=d) then True else False


convertConstraintInCharIdx (b,e) tokss =
  let tb' = find (\t -> (t ^. token_tok_idx_range ^. _1) == b) (concat tokss)
      te' = find (\t -> (t ^. token_tok_idx_range ^. _2) == e) (concat tokss)
  in case (tb',te') of
    (Just tb, Just te) -> Just $ (tb ^. token_char_idx_range ^. _1, te ^. token_char_idx_range ^. _2)
    otherwise          -> Nothing

findNETokens con tokss = filter (\t -> (t ^. token_char_idx_range) `contained` con) (concat tokss)

mkNewWikiEL cons tokss = do
  result <- flip evalStateT (0 :: Int) $ do
    forM cons $ \(a,b) -> do
      s <- get
      let ff (a,b) = ((*) 6 $ length $ filter (\t -> '.' `elem` (T.unpack (t ^. token_text))) $ findNETokens (a,b) tokss ) +
                     ((*) 2 $ length $ filter (\t -> '&' `elem` (T.unpack (t ^. token_text))) $ findNETokens (a,b) tokss )
      modify' $ (\s' -> s' + (ff (a,b)))
      return (a + s, b + s + ff (a,b))
  return result
