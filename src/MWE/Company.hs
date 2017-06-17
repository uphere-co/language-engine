{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MWE.Company where

import           Data.Text                    (Text)
import qualified Data.Text              as T
import           Data.Tree
--
import           Generic.SearchTree


makeF7745Forest :: Text -> Forest (Maybe Char)
makeF7745Forest txt =
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
  in forest



{-
main' :: IO ()
main' = do
  putStrLn "search"
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
      forest = foldr addTreeItem [] nentities
      testtxt = "I think Intel will be the most successful in history."
  print (parseOnly (many (pTreeAdv forest)) testtxt)
-}
