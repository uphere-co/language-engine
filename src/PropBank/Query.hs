{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module PropBank.Query where

import           Control.Lens
import           Control.Monad             (foldM)
import           Data.Foldable             (toList)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (foldl',sort)
import           Data.Text                 (Text)
import qualified Data.Text           as T
import           System.Directory          (getDirectoryContents)
import           System.FilePath           ((</>),takeBaseName,takeExtensions)
--
import           PropBank.Parser
import           PropBank.Type

newtype FrameDB = FrameDB { _frameDB :: HashMap Text [Predicate] }

makeLenses ''FrameDB

newtype PredicateDB = PredicateDB { _predicateDB :: HashMap Text Predicate }

makeLenses ''PredicateDB

constructFrameDB :: FilePath -> IO FrameDB
constructFrameDB dir = do
  cnts <- getDirectoryContents dir
  let flst = sort (filter (\x -> takeExtensions x == ".xml") cnts)
  FrameDB <$> foldM action HM.empty flst
 where
  action m f = do
    -- putStrLn f
    e <- parseFrameFile (dir </> f)
    case e of
      Left err -> putStrLn err >> return m
      Right fr -> return $ HM.insert (T.pack (takeBaseName f)) (fr^.frameset_predicate) m

constructPredicateDBFromFrameDB :: FrameDB -> PredicateDB
constructPredicateDBFromFrameDB (FrameDB m) = PredicateDB (foldl' f HM.empty (concat (toList m)))
  where f !acc !x = HM.insert (x^.predicate_lemma) x acc
        
