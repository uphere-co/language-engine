{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SRL.CoNLL.CoNLL08.Parser where

import           Control.Lens
import           Data.List           (transpose)
import           Data.List.Split     (splitWhen)
import           Data.Maybe          (catMaybes, isJust)
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tuple          (swap)
--
import           SRL.CoNLL.CoNLL08.Type

parseDeprel :: Text -> Deprel
parseDeprel "ADV"   = ADV
parseDeprel "AMOD"  = AMOD
parseDeprel "APPO"  = APPO
parseDeprel "BNF"   = BNF
parseDeprel "CONJ"  = CONJ
parseDeprel "COORD" = COORD
parseDeprel "DEP"   = DEP
parseDeprel "DTV"   = DTV
parseDeprel "EXTR"  = EXTR
parseDeprel "GAP"   = GAP
parseDeprel "HMOD"  = HMOD
parseDeprel "LGS"   = LGS
parseDeprel "LOC"   = LOC
parseDeprel "MNR"   = MNR
parseDeprel "NMOD"  = NMOD
parseDeprel "OBJ"   = OBJ
parseDeprel "OPRD"  = OPRD
parseDeprel "P"     = P 
parseDeprel "PMOD"  = PMOD
parseDeprel "POSTHON" = POSTHON
parseDeprel "PRD"   = PRD
parseDeprel "PRN"   = PRN
parseDeprel "PRP"   = PRP
parseDeprel "PRT"   = PRT
parseDeprel "PUT"   = PUT
parseDeprel "ROOT"  = ROOT
parseDeprel "SBJ"   = SBJ
parseDeprel "SUB"   = SUB
parseDeprel "SUFFIX" = SUFFIX
parseDeprel "TITLE" = TITLE
parseDeprel "TMP"   = TMP
parseDeprel "VOC"   = VOC
parseDeprel "VC"    = VC
--
parseDeprel "ADV-GAP"     = ADV_GAP
parseDeprel "AMOD-GAP"    = AMOD_GAP
parseDeprel "DEP-GAP"     = DEP_GAP
parseDeprel "DIR"         = DIR
parseDeprel "DIR-GAP"     = DIR_GAP 
parseDeprel "DTV-GAP"     = DTV_GAP
parseDeprel "END"         = END
parseDeprel "EXT"         = EXT
parseDeprel "EXT-GAP"     = EXT_GAP
parseDeprel "EXTR-GAP"    = EXTR_GAP
parseDeprel "GAP-LGS"     = GAP_LGS
parseDeprel "GAP-LOC"     = GAP_LOC
parseDeprel "GAP-LOC-PRD" = GAP_LOC_PRD
parseDeprel "GAP-MNR"     = GAP_MNR
parseDeprel "GAP-NMOD"    = GAP_NMOD
parseDeprel "GAP-OBJ"     = GAP_OBJ
parseDeprel "GAP-OPRD"    = GAP_OPRD
parseDeprel "GAP-PMOD"    = GAP_PMOD
parseDeprel "GAP-PRD"     = GAP_PRD
parseDeprel "GAP-PRP"     = GAP_PRP
parseDeprel "GAP-PUT"     = GAP_PUT
parseDeprel "GAP-SBJ"     = GAP_SBJ
parseDeprel "GAP-SUB"     = GAP_SUB
parseDeprel "GAP-TMP"     = GAP_TMP
parseDeprel "GAP-VC"      = GAP_VC
parseDeprel "HYPH"        = HYPH
parseDeprel "IM"          = IM
parseDeprel "LOC-MNR"     = LOC_MNR
parseDeprel "LOC-OPRD"    = LOC_OPRD
parseDeprel "LOC-PRD"     = LOC_PRD
parseDeprel "LOC-TMP"     = LOC_TMP
parseDeprel "MNR-PRD"     = MNR_PRD
parseDeprel "MNR-TMP"     = MNR_TMP
parseDeprel "NAME"        = NAME
parseDeprel "PRD-PRP"     = PRD_PRP
parseDeprel "PRD-TMP"     = PRD_TMP
parseDeprel x       = error ("parseDeprel: " ++ (T.unpack x))


  
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
       , _line_deprel      :: Deprel -- ^ Syntactic dependency relation to the HEAD. The syntactic dependency
                                     --   analysis is very similar to that used for the English data sets in the
                                     --   CoNLL 2007 shared task and is further described here.
       , _line_pred        :: Maybe Text
                                     -- ^ Rolesets of the semantic predicates in this sentence. This includes both
                                     --   nominal and verbal predicates. The split-form tokens that are not
                                     --   semantic predicates must be marked with "_". We use the same roleset
                                     --   names as the PropBank and NomBank frames.
       , _line_args        :: [Maybe Text]
                                     -- ^ Columns with argument labels for the each semantic predicate following
                                     --   textual order, i.e., the first column corresponds to the first predicate
                                     --   in PRED, the second column to the second predicate, etc. Note that,
                                     --   because this algorithm uniquely identifies the ID of the corresponding
                                     --   predicate, it is sufficient to store the label of the argument here.
                                     --   The argument labels for verbal predicates follow the PropBank conventions.
                                     --   Labels of arguments to nominal predicates use NomBank conventions.
       }
  deriving (Show,Ord,Eq)

makeLenses ''Line

type RoleSet = (Int,Text) -- ,[Arg])


type Arg = (Text,Int)


data Sentence = Sentence { _sentence_lines :: [Line]
                         , _sentence_preds :: [(RoleSet,[Arg])]
                         }
              deriving (Show,Ord,Eq)

makeLenses ''Sentence

parseLine :: Text -> Line
parseLine txt =
  let _line_id:_line_form:_line_lemma:_line_gpos:_line_ppos:_line_split_form:_line_split_lemma:_line_pposs:_line_head:_line_deprel':_line_pred':_line_args' = T.split (== '\t') txt
      _line_deprel = parseDeprel _line_deprel'
      _line_pred = if _line_pred' == "_" then Nothing else Just _line_pred'
      _line_args = map (\x -> if x == "_" then Nothing else Just x) _line_args' 
  in Line {..}

parseArgs :: [Maybe Text] -> [Arg]
parseArgs ws = map swap $ takeJustAfterEnum ws

takeJustAfterEnum = catMaybes . zipWith (\x y -> (x,) <$> y) [1..]
  
parseSentence :: [Text] -> Sentence
parseSentence txts = let ls = map parseLine txts
                         preds =  takeJustAfterEnum (map (view line_pred) ls)
                         predargs = zip preds . map parseArgs . transpose . map (view line_args) $ ls
                     in Sentence ls predargs
                         

parseFile :: FilePath -> IO [Sentence]
parseFile fp = do
  putStrLn $ "parsing " ++ fp
  txt <- TIO.readFile fp
  let xs = T.lines txt
      ys = splitWhen T.null xs
  return (map parseSentence ys)
