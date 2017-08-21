{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lexicon.Mapping.Type where

import           Control.Lens         (makeLenses)
import           Data.Hashable        (Hashable)
import           Data.Text            (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           GHC.Generics         (Generic)


type SenseID = (Text,Text)

type PBArg = Text

type FNFrameElement = Text


type RoleMapInstance = (SenseID,[(PBArg,FNFrameElement)])



-----------------------------
-- ArgTable and ArgPattern --
-----------------------------



-- | ArgTable node that allows simple or linked node
--
data ATNode a = SimpleNode { _atnode_orig :: a }
              | LinkedNode { _atnode_orig  :: a
                           , _atnode_link  :: a }
              deriving (Show,Functor,Foldable,Traversable)



-- now i can experiment flexibly with linked node
chooseATNode (SimpleNode x) = x
chooseATNode (LinkedNode x y) = x

data ArgTable b = ArgTable { _tbl_rel  :: Maybe Text
                           , _tbl_arg0 :: Maybe b
                           , _tbl_arg1 :: Maybe b
                           , _tbl_arg2 :: Maybe b
                           , _tbl_arg3 :: Maybe b
                           , _tbl_arg4 :: Maybe b
                           , _tbl_file_sid_tid :: (FilePath,Int,Int)
                           }
                  deriving (Show,Functor,Foldable,Traversable)

makeLenses ''ArgTable



data ArgPattern p a = ArgPattern { _patt_property :: Maybe p
                                 , _patt_arg0 :: Maybe a
                                 , _patt_arg1 :: Maybe a
                                 , _patt_arg2 :: Maybe a
                                 , _patt_arg3 :: Maybe a
                                 , _patt_arg4 :: Maybe a
                                 }
                    deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)

makeLenses ''ArgPattern


instance (Hashable p) => Hashable (ArgPattern p Text)





parseRoleMap :: [Text] -> RoleMapInstance
parseRoleMap (i:lma:sense:frame:rest) = let lst = map (\w -> let x:y:_ = T.splitOn ":" w in (x,y)) rest
                                        in ((lma,sense),("frame",frame):lst)


loadRoleMap :: FilePath -> IO [RoleMapInstance]
loadRoleMap fp = do
  txt <- T.IO.readFile fp
  let rolemap = map parseRoleMap . map T.words . T.lines $ txt
  return rolemap
