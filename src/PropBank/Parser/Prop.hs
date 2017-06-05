{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module PropBank.Parser.Prop where

import           Control.Applicative         (many,(<|>))
import           Control.Lens
import qualified Data.Attoparsec.Text as A
import           Data.Foldable               (toList)
import qualified Data.List            as L   (lookup)
import           Data.Maybe                  (listToMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Text.Read              (decimal)
--
import           NLP.Type.PennTreebankII     (PennTreeGen(..),PennTree,mkIndexedTree,contain)
--
import           PropBank.Type.Prop


readDecimal :: Text -> Int
readDecimal x = case decimal x of {Left err -> error err; Right (n,_) -> n } 

parseInst :: Text -> Instance
parseInst txt =
  let _inst_tree_id':_inst_predicate_id':_inst_annotator_id:_inst_lemma_type:_inst_lemma_roleset_id:_:_inst_arguments'
        = T.words txt
      _inst_tree_id = readDecimal _inst_tree_id'
      _inst_predicate_id = readDecimal _inst_predicate_id'
      _inst_arguments = case mapM parseArg _inst_arguments' of
                          Nothing -> error ("parseArg : " ++ T.unpack txt) -- "parseArg"
                          Just xs -> xs
  in Instance {..}

parseInstOmit :: Text -> Instance
parseInstOmit txt =
  let _inst_tree_id':_inst_predicate_id':_inst_annotator_id:_inst_lemma_roleset_id:_:_inst_arguments'
        = T.words txt
      _inst_lemma_type = ""        
      _inst_tree_id = readDecimal _inst_tree_id'
      _inst_predicate_id = readDecimal _inst_predicate_id'
      _inst_arguments = case mapM parseArg _inst_arguments' of
                          Nothing -> error "parseArg"
                          Just xs -> xs
  in Instance {..}



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
  (A.string "PRD" >> return PRD) <|>
  (A.string "PRP" >> return PRP) <|>
  (A.string "PRR" >> return PRR) <|>
  (A.string "REC" >> return REC) <|>
  (A.string "TMP" >> return TMP)

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
   
parseProp :: IsOmit -> Text -> [Instance]
parseProp omit = case omit of
                   NoOmit -> map parseInst . T.lines
                   Omit -> map parseInstOmit . T.lines

parseNomProp :: Text -> [NomInstance]
parseNomProp = map parseNomInst . T.lines


-- showPropBankLabel :: PropBankLabel 


findNodePathForLeaf :: Int -> PennTree -> [PennTreeGen Text (Int,(Text,Text))]
findNodePathForLeaf i tr = contain i (mkIndexedTree tr)

findNode :: Node -> PennTree -> Maybe (Text, PennTreeGen Text (Int,(Text,Text)))
findNode (Node i d) tr = do
  let lst = reverse (findNodePathForLeaf i tr)
  PL (_,(headword,_)) <- listToMaybe (take 1 lst)
  r <- listToMaybe $ drop d lst
  return (headword,r)

showInstance :: (PennTree,Instance) -> IO ()
showInstance (tr,prop) = do
  TIO.putStrLn "---------------"
  TIO.putStrLn (prop^.inst_lemma_roleset_id)
  TIO.putStrLn "---------------"
  mapM_ (showArgument tr) (prop^.inst_arguments)

showNomInstance :: (PennTree,NomInstance) -> IO ()
showNomInstance (tr,nom) = do
  TIO.putStrLn "---------------"
  showNomSense (tr,nom)
  TIO.putStrLn "---------------"
  mapM_ (showArgument tr) (nom^.nominst_arguments)


showNomSense :: (PennTree,NomInstance) -> IO ()
showNomSense (tr,nom) = do
  let itr = zip [0..] $ toList tr
  print itr
  print (nom^.nominst_predicate_id)
  case L.lookup (nom^.nominst_predicate_id) $ zip [0..] $ toList tr of
    Nothing -> putStrLn "non-sense?"
    Just (_,n) -> TIO.putStrLn (n <> "." <> T.pack (show (nom^.nominst_sense_number)))
  
showArgument :: PennTree -> Argument -> IO ()
showArgument tr arg = do
  putStr (show (arg^.arg_label) <> ": ")
  let format (t,n) = "(" <> t <> ") " <>   (T.intercalate " " . map (^._2._2) . toList) n
  mapM_ (\x -> TIO.putStr (maybe "Nothing" format (findNode x tr)) >> TIO.putStr ", ") (arg^.arg_terminals)
  TIO.putStr "\n"

showSentenceProp :: (Int,(PennTree,[Instance])) -> IO ()
showSentenceProp (i,(tr,props)) = do
  TIO.putStrLn "================================================="
  TIO.putStr ("Sentence " <> T.pack (show i) <> ": ") 
  (TIO.putStrLn . T.intercalate " " . map (^._2) . toList) tr
  mapM_ (showInstance . (tr,)) props

showSentenceNom :: (Int,(PennTree,[NomInstance])) -> IO ()
showSentenceNom (i,(tr,props)) = do
  TIO.putStrLn "================================================="
  TIO.putStr ("Sentence " <> T.pack (show i) <> ": ") 
  (TIO.putStrLn . T.intercalate " " . map (^._2) . toList) tr
  mapM_ (showNomInstance . (tr,)) props

