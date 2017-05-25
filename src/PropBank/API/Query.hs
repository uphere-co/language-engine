{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropBank.API.Query where

import           Control.Lens
import           Data.Function              (on)
import qualified Data.HashMap.Strict as HM
import           Data.List                  (sortBy)
import           Data.Maybe                 (fromMaybe,maybeToList)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.Builder     as TLB  (toLazyText)
import qualified Data.Text.Lazy.IO   as TLIO
--
import           YAML.Builder
--
import           PropBank.Query
import           PropBank.Type

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
