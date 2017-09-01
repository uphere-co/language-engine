{-# LANGUAGE OverloadedStrings   #-}

module MWE.Util where

import           Control.Lens
import           Control.Monad.State
import           Data.Maybe          (catMaybes)
import qualified Data.Text           as T
--
import           NLP.Type.CoreNLP


mkTextFromToken :: [Maybe Token] -> T.Text
mkTextFromToken mtoks =
  let toks = catMaybes mtoks
      txts = join $ flip evalStateT 0 $ forM toks $ \t -> do
          s <- get
          modify' $ (\s -> t ^. token_char_idx_range ^. _2)
          return (T.append (T.replicate ((t ^. token_char_idx_range ^. _1) - s) " ") (t ^. token_text))
  in T.intercalate "" txts
