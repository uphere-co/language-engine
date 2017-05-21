{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Loops        (whileJust_)
import           Data.Function              (on)
import qualified Data.HashMap.Strict as HM
import           Data.List                  (sortBy)
import           Data.Maybe                 (catMaybes,fromMaybe,maybeToList)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB  (toLazyText)
import qualified Data.Text.Lazy.IO          as TLIO
import           Data.Text.Read             (decimal)
import qualified Options.Applicative as O
import           System.Console.Haskeline
import           System.FilePath            ((</>))
--
import           YAML.Builder
--
import           PropBank



instance MakeYaml Int where
  makeYaml _ x = YPrim (YInteger x)

instance MakeYaml (Int,Int) where
  makeYaml n (x,y) = YLArray Inline [ makeYaml n x, makeYaml n y ] 

instance MakeYaml Text where
  makeYaml _ txt = YPrim (YString Plain (TL.fromStrict txt))


inj :: k -> a -> [(k,a)]
inj k x = [(k,x)]

single n k = maybe [] (inj k . makeYaml n)

instance MakeYaml RoleSet where
  makeYaml n s =
    YObject $
      [("id"     , makeYaml n (s^.roleset_id))]
      <> single n "name"  (s^.roleset_name)
      <> single n "source" (s^.roleset_source)
      <> single n "vncls" (s^.roleset_vncls)
      <> single n "roleset" (s^.roleset_roleset)
      <> single n "framenet" (s^.roleset_framenet)
      <> [("roles", makeYaml n (s^.roleset_roles))]

instance MakeYaml Roles where
  makeYaml n s = YIArray (map (makeYaml n) (s^.roles_role))


instance MakeYaml Role where
  makeYaml n s =
    YObject $ [("n", makeYaml n (s^.role_n))]
              <> single n "f" (s^.role_f)
              <> single n "source" (s^.role_source)
              <> single n "description" (s^.role_descr)
              <> [("vnrole", YIArray (map (makeYaml n) (s^.role_vnrole)))]

instance MakeYaml VNRole where
  makeYaml n s =
    YObject $ [ ("vncls",makeYaml n (s^.vnrole_vncls))
              , ("vntheta",makeYaml n (T.pack (show (s^.vntheta)))) 
              ]

data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "PropBank lookup")




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

queryRoleSet db input = do
  case HM.lookup input (db^.rolesetDB) of
    Nothing -> putStrLn "No such roleset"
    Just r ->  TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 r))
  
main = do
  opt <- O.execParser progOption
  pdb <- constructPredicateDB <$> constructFrameDB (dir opt)
  let rdb = constructRoleSetDB pdb
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ 
    let input = T.pack input'
    in case T.split (== '.') input of
         (x:n:_) -> queryRoleSet rdb input
         (x:[])  -> queryPredicate pdb input
         [] -> putStrLn "query is not recognized."
