{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module PropBank.Query where

import           Control.Lens
import           Control.Monad             (foldM)
import           Data.Foldable             (toList)
import           Data.Function             (on)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (foldl',sort,sortBy)
import           Data.Maybe                (fromMaybe,maybeToList)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.Builder     as TLB  (toLazyText)
import qualified Data.Text.Lazy.IO   as TLIO
import           System.Directory          (getDirectoryContents)
import           System.FilePath           ((</>),takeBaseName,takeExtensions)
--
import           YAML.Builder
--
import           PropBank.Parser.Frame
import           PropBank.Type

newtype FrameDB = FrameDB { _frameDB :: HashMap Text [Predicate] }

makeLenses ''FrameDB

newtype PredicateDB = PredicateDB { _predicateDB :: HashMap Text Predicate }

makeLenses ''PredicateDB

newtype RoleSetDB = RoleSetDB { _rolesetDB :: HashMap Text RoleSet }

makeLenses ''RoleSetDB

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

constructPredicateDB :: FrameDB -> PredicateDB
constructPredicateDB (FrameDB m) = PredicateDB (foldl' f HM.empty (concat (toList m)))
  where f !acc !x = HM.insert (x^.predicate_lemma) x acc

constructRoleSetDB :: PredicateDB -> RoleSetDB 
constructRoleSetDB (PredicateDB m) = RoleSetDB (foldl' f HM.empty (concatMap (^.predicate_roleset) (toList m)))
  where f !acc !x = HM.insert (x^.roleset_id) x acc

queryPredicate :: PredicateDB -> Text -> IO ()
queryPredicate db input = do
  let result = do
        p <- maybeToList (HM.lookup input (db^.predicateDB))
        r <- p ^. predicate_roleset
        let (i,n) = (r^.roleset_id,fromMaybe "" (r^.roleset_name))
        return (i,n)
        -- p ^.. (predicate_roleset . traverse . roleset_id)
  if null result
    then putStrLn "No such predicate"
    else mapM_ (\(i,n) -> TIO.putStrLn (i <> "\t" <> n)) (sortBy (compare `on` fst) result)

queryRoleSet :: RoleSetDB -> Text -> IO ()
queryRoleSet db input = do
  case HM.lookup input (db^.rolesetDB) of
    Nothing -> putStrLn "No such roleset"
    Just r ->  TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 r))
