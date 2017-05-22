{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.CoNLL.Parser where

import           Control.Lens
import           Data.List.Split     (splitWhen)
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Line =
  Line { _line_id          :: Text   -- ^ Token counter, starting at 1 for each new sentence.
       , _line_form        :: Text   -- ^ Word form or punctuation symbol. The FORM field uses the original
                                     --   WSJ tokenization, i.e., hyphenated words such as "Atlanta-based"
                                     --   are not split.
       , _line_lemma       :: Text   -- ^ Predicted lemma of FORM, extracted from WordNet using the most common
                                     --   sense of the word. If FORM does not exist in WordNet, LEMMA is set to
                                     --   the lower-case version of FORM.
       , _line_gpos        :: Text   -- ^ Gold part-of-speech (POS) tag from the TreeBank. Note that, in order
                                     --   to have a realistic evaluation, this field is provided only for the
                                     --   training and development sets. For the testing sets this column will
                                     --   contain "_".
       , _line_ppos        :: Text   -- ^ Predicted POS tag. These tags are predicted by a state-of-the-art POS
                                     --   tagger. To avoid overfitting the tagger on the training corpus the
                                     --   tags on the training set are generated through 10-fold cross-validation.
       , _line_split_form  :: Text   -- ^ Tokens split at hyphens and slashes. Because some arguments, generally
                                     --   arguments of nominal predicates, may appear inside hyphenated words it
                                     --   is necessary to split hyphenated constructs in order to correctly
                                     --   annotate such arguments. For example, the TreeBank token "Atlanta-based"
                                     --   is split into three SPLIT_FORMs: "Atlanta", "-", and "based". To ensure
                                     --   the same number of rows for all columns corresponding to one sentence,
                                     --   the FORM, LEMMA, GPOS, and PPOS columns are padded with "_" fields for
                                     --   all split tokens. 
       , _line_split_lemma :: Text   -- ^ Predicted lemma of SPLIT_FORM, extracted from WordNet using the most
                                     --   common sense of the word. If SPLIT_FORM does not exist in WordNet,
                                     --   SPLIT_LEMMA is set to the lower-case version of SPLIT_FORM.
       , _line_pposs       :: Text   -- ^ Predicted POS tags of the split forms. These tags are generated using
                                     --   the same state-of-the-art tagger and cross-validation process as PPOS.
       , _line_head        :: Text   -- ^ Syntactic head of the current token, which is either a value of ID or
                                     --   zero ("0"). Note that both syntactic and semantic dependencies annotate
                                     --   the split-form tokens.
       , _line_deprel      :: Text   -- ^ Syntactic dependency relation to the HEAD. The syntactic dependency
                                     --   analysis is very similar to that used for the English data sets in the
                                     --   CoNLL 2007 shared task and is further described here.
       , _line_pred        :: Text   -- ^ Rolesets of the semantic predicates in this sentence. This includes both
                                     --   nominal and verbal predicates. The split-form tokens that are not
                                     --   semantic predicates must be marked with "_". We use the same roleset
                                     --   names as the PropBank and NomBank frames.
       , _line_args        :: [Text] -- ^ Columns with argument labels for the each semantic predicate following
                                     --   textual order, i.e., the first column corresponds to the first predicate
                                     --   in PRED, the second column to the second predicate, etc. Note that,
                                     --   because this algorithm uniquely identifies the ID of the corresponding
                                     --   predicate, it is sufficient to store the label of the argument here.
                                     --   The argument labels for verbal predicates follow the PropBank conventions.
                                     --   Labels of arguments to nominal predicates use NomBank conventions.
       }
  deriving (Show,Ord,Eq)

makeLenses ''Line

newtype Sentence = Sentence { _sentence_lines :: [Line] }
                 deriving (Show,Ord,Eq)

makeLenses ''Sentence

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
