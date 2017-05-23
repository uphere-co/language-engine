{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropBank.Parser.Prop where

import           Control.Applicative       (many)
import qualified Data.Attoparsec.Text as A
import           Data.Text                 (Text)
import qualified Data.Text            as T
import           Data.Text.Read            (decimal)

data Node = Node { _node_id :: Int
                 , _node_height :: Int }
          deriving (Show,Eq,Ord)

data Argument = Argument { _arg_terminals :: [Node]
                         , _arg_label       :: Text
                         }
              deriving (Show,Eq,Ord)
                         
data Instance = Instance { _inst_tree_id      :: Int
                         , _inst_predicate_id :: Int
                         , _inst_annotator_id :: Text
                         , _inst_lemma_type       :: Text
                         , _inst_lemma_roleset_id :: Text
                         , _inst_arguments :: [Argument]
                         }
              deriving (Show,Eq,Ord)

readDecimal x = case decimal x of {Left err -> error err; Right (n,_) -> n } 

parseInst :: Text -> Instance
parseInst txt =
  let _inst_tree_id':_inst_predicate_id':_inst_annotator_id:_inst_lemma_type:_inst_lemma_roleset_id:_:_inst_arguments'
        = T.words txt
      _inst_tree_id = readDecimal _inst_tree_id'
      _inst_predicate_id = readDecimal _inst_predicate_id'
      _inst_arguments = case mapM parseArg _inst_arguments' of
                          Nothing -> error "parseArg"
                          Just xs -> xs
  in Instance {..}

parseArg :: Text -> Maybe Argument
parseArg txt = case A.parseOnly p_arg txt of
                 Left _ -> Nothing
                 Right x -> Just x
  where p_arg = do ns <- p_node `A.sepBy` A.char '*'
                   A.char '-'
                   l <- T.pack <$> many A.anyChar
                   return (Argument ns l)
        p_node = do i <- A.takeTill (== ':')
                    A.char ':'
                    h <- A.takeTill (`elem`  ['-', '*'])
                    return (Node (readDecimal i) (readDecimal h))

        
parseProp :: Text -> [Instance]
parseProp = map parseInst . T.lines


