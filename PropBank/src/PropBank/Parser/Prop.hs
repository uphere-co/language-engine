{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module PropBank.Parser.Prop where

import           Control.Applicative         ((<|>))
import qualified Data.Attoparsec.Text as A
import           Data.Text                   (Text)
import qualified Data.Text            as T
import           Data.Text.Read              (decimal)
--
import           PropBank.Type.Prop


readDecimal :: Text -> Int
readDecimal x = case decimal x of {Left err -> error err; Right (n,_) -> n } 


parseInst :: Text -> Instance
parseInst txt =
  let _inst_tree_id':_inst_predicate_id':_inst_annotator_id:_inst_lemma_type:_inst_lemma_roleset_id':_:_inst_arguments'
        = T.words txt
      _inst_tree_id = readDecimal _inst_tree_id'
      _inst_predicate_id = readDecimal _inst_predicate_id'
      _inst_lemma_roleset_id = case parseRoleSetId _inst_lemma_roleset_id' of
                                 Nothing -> error ("parseRoleSetId: " ++ T.unpack txt)
                                 Just xs -> xs
      _inst_arguments = case mapM parseArg _inst_arguments' of
                          Nothing -> error ("parseArg: " ++ T.unpack txt)
                          Just xs -> xs
  in Instance {..}

parseInstOmit :: Text -> Instance
parseInstOmit txt =
  let _inst_tree_id':_inst_predicate_id':_inst_annotator_id:_inst_lemma_roleset_id':_:_inst_arguments'
        = T.words txt
      _inst_lemma_type = ""        
      _inst_tree_id = readDecimal _inst_tree_id'
      _inst_predicate_id = readDecimal _inst_predicate_id'
      _inst_lemma_roleset_id = case parseRoleSetId _inst_lemma_roleset_id' of
                                 Nothing -> error ("parseRoleSetId: " ++ T.unpack txt)
                                 Just xs -> xs      
      _inst_arguments = case mapM parseArg _inst_arguments' of
                          Nothing -> error ("parseArg: " ++ show txt)
                          Just xs -> xs
  in Instance {..}


parsePropWithFileField :: IsOmit -> Text -> [Instance]
parsePropWithFileField omit txt =
  case omit of
    NoOmit -> (map (parseInst . T.intercalate " " . tail . T.words) . T.lines) txt
    Omit -> (map (parseInstOmit . T.intercalate " " . tail . T.words) . T.lines) txt


parseNomInst :: Text -> NomInstance
parseNomInst txt = 
  let _nominst_tree_file':_nominst_tree_id':_nominst_predicate_id':_nominst_base_form:_nominst_sense_number':_nominst_arguments'
        = T.words txt
      _nominst_tree_file = T.unpack _nominst_tree_file'
      _nominst_tree_id = readDecimal _nominst_tree_id'
      _nominst_predicate_id = readDecimal _nominst_predicate_id'
      _nominst_sense_number = readDecimal _nominst_sense_number'
      _nominst_arguments = case mapM parseArg _nominst_arguments' of
                             Nothing -> error "parseArg"
                             Just xs -> xs
  in NomInstance {..}

parseRoleSetId :: Text -> Maybe (Text,Text)
parseRoleSetId txt = case A.parseOnly p txt of
                       Left _ -> Nothing
                       Right x -> Just x
  where p = do lemma <- A.takeTill (== '.')
               A.char '.'
               sensenum <- A.takeTill (== ' ')
               return (lemma,sensenum)

parseArg :: Text -> Maybe Argument
parseArg txt = case A.parseOnly p_arg txt of
                 Left _ -> Nothing
                 Right x -> Just x
  where p_arg = do ns <- p_node `A.sepBy` A.char '*'
                   A.char '-'
                   -- l <- T.pack <$> many A.anyChar
                   l <- parsePropBankLabel 
                   return (Argument ns l)
        p_node = do i <- A.takeTill (== ':')
                    A.char ':'
                    h <- A.takeTill (`elem`  ['-', '*'])
                    return (Node (readDecimal i) (readDecimal h))

parseModifierType :: A.Parser ModifierType
parseModifierType = 
  (A.string "ADJ" >> return ADJ) <|>
  (A.string "ADV" >> return ADV) <|>
  (A.string "CAU" >> return CAU) <|>
  (A.string "COM" >> return COM) <|>
  (A.string "DIR" >> return DIR) <|>
  (A.string "DIS" >> return DIS) <|>
  (A.string "DSP" >> return DSP) <|>
  (A.string "EXT" >> return EXT) <|>
  (A.string "GOL" >> return GOL) <|>
  (A.string "LOC" >> return LOC) <|>
  (A.string "MNR" >> return MNR) <|>
  (A.string "MOD" >> return MOD) <|>
  (A.string "NEG" >> return NEG) <|>
  (A.string "PNC" >> return PNC) <|>
  (A.string "PRD" >> return PRD) <|>
  (A.string "PRP" >> return PRP) <|>
  (A.string "PRR" >> return PRR) <|>
  (A.string "REC" >> return REC) <|>
  (A.string "TMP" >> return TMP) <|>
  (A.string "LVB" >> return LVB) <|>
  (A.string "PRX" >> return PRX) 


parseLinkType :: A.Parser LinkType
parseLinkType =
  (A.string "PRO" >> return PRO) <|>
  (A.string "PSV" >> return PSV) <|>
  (A.string "SLC" >> return SLC) <|>
  (A.string "PCR" >> return PCR)


parsePropBankLabel :: A.Parser PropBankLabel
parsePropBankLabel =
  (do A.string "rel"
      return Relation)
  <|>
  (do A.string "ARG"
      ((NumberedArgument <$> A.decimal) <|>
       (A.char 'M' >> A.char '-' >> (Modifier <$> parseModifierType)))
  )
  <|>
  (do A.string "LINK-"
      LinkArgument <$> parseLinkType
  )
  <|>
  (do A.string "ARGA"
      return (NumberedArgument 0))  -- error case VOL15_3.prop

      
  
parseProp :: IsOmit -> Text -> [Instance]
parseProp omit = case omit of
                   NoOmit -> map parseInst . T.lines
                   Omit -> map parseInstOmit . T.lines

parseNomProp :: Text -> [NomInstance]
parseNomProp = map parseNomInst . T.lines
