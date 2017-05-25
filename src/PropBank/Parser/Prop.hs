{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PropBank.Parser.Prop where

import           Control.Applicative         (many)
import           Control.Lens
import           Control.Monad.Trans.State
import qualified Data.Attoparsec.Text as A
import           Data.Foldable               (toList)
import           Data.Maybe                  (listToMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Text.Read              (decimal)
--
import           NLP.Type.PennTreebankII     (PennTreeGen(..),PennTree)
--
import           PropBank.Type.Prop


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



mkIndexedTree :: PennTree -> PennTreeGen Text Text (Int,Text)
mkIndexedTree tr = evalState (traverse tagidx tr) 0
  where tagidx x = get >>= \n -> put (n+1) >> return (n,x)
        

contain :: Int -> PennTreeGen Text Text (Int,Text) -> [PennTreeGen Text Text (Int,Text)]
contain i y@(PN _ xs) = case (filter (not.null) . map (contain i)) xs of
                          [] -> []
                          ys:_ -> y:ys
contain i x@(PL _ (j,_)) | i == j = [x]
                         | otherwise = []

findNodePathForLeaf :: Int -> PennTree -> [PennTreeGen Text Text (Int,Text)]
findNodePathForLeaf i tr = contain i (mkIndexedTree tr)

findNode :: Node -> PennTree -> Maybe (Text, PennTreeGen Text Text (Int,Text))
findNode (Node i d) tr = do
  let lst = reverse (findNodePathForLeaf i tr)
  PL _ (_,headword) <- listToMaybe (take 1 lst)
  r <- listToMaybe $ drop d lst
  return (headword,r)

showInstance :: (PennTree,Instance) -> IO ()
showInstance (tr,prop) = do
  TIO.putStrLn "---------------"
  TIO.putStrLn (prop^.inst_lemma_roleset_id)
  TIO.putStrLn "---------------"
  mapM_ (showArgument tr) (prop^.inst_arguments)
  
showArgument :: PennTree -> Argument -> IO ()
showArgument tr arg = do
  TIO.putStr (arg^.arg_label <> ": ")
  let format (t,n) = "(" <> t <> ") " <>   (T.intercalate " " . map snd . toList) n
  mapM_ (\x -> TIO.putStr (maybe "Nothing" format (findNode x tr)) >> TIO.putStr ", ") (arg^.arg_terminals)
  TIO.putStr "\n"


