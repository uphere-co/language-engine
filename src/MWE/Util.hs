{-# LANGUAGE OverloadedStrings   #-}

module MWE.Util where

import           Control.Lens
import           Control.Monad.State
import           Data.Maybe          (catMaybes)
import qualified Data.Text           as T
--
import           NLP.Type.CoreNLP

mkTextFromToken mtoks = do
  let toks = catMaybes mtoks
  result <- flip evalStateT (0 :: Int) $ do
    forM toks $ \t -> do
      s <- get
      modify' $ (\s -> t ^. token_char_idx_range ^. _2)
      return (T.append (T.replicate ((t ^. token_char_idx_range ^. _1) - s) " ") (t ^. token_text))
  return $ T.intercalate "" result
