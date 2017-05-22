{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SRL.CoNLL.Parser where

import           Data.List.Split     (splitWhen)
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Line = Line { _line_id          :: Text
                 , _line_form        :: Text 
                 , _line_lemma       :: Text
                 , _line_gpos        :: Text 
                 , _line_ppos        :: Text
                 , _line_split_form  :: Text
                 , _line_split_lemma :: Text
                 , _line_pposs       :: Text
                 , _line_head        :: Text
                 , _line_deprel      :: Text
                 , _line_pred        :: Text
                 , _line_args        :: [Text]
                 }
          deriving (Show,Ord,Eq)

newtype Sentence = Sentence [Line]
                 deriving (Show,Ord,Eq)

parseLine :: Text -> Line
parseLine txt =
  let _line_id:_line_form:_line_lemma:_line_gpos:_line_ppos:_line_split_form:_line_split_lemma:_line_pposs:_line_head:_line_deprel:_line_pred:_line_args = T.split (== '\t') txt
  in Line {..}

     
parseSentence :: [Text] -> Sentence
parseSentence = Sentence . map parseLine

parseFile :: FilePath -> IO ()
parseFile fp = do
  putStrLn $ "parsing " ++ fp
  txt <- TIO.readFile fp
  let xs = T.lines txt
      ys = splitWhen T.null xs
      zs =map parseSentence ys
  mapM_ print zs
