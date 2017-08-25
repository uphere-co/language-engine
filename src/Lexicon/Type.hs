{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lexicon.Type where

import           Control.Lens         (makeLenses)
import           Data.Hashable        (Hashable)
import           Data.Text            (Text)
-- import qualified Data.Text    as T
-- import qualified Data.Text.IO as T.IO
import           GHC.Generics         (Generic)


data POSVorN = Verb | Noun deriving (Show,Eq,Ord,Generic)

instance Hashable POSVorN


type SenseID = (Text,POSVorN,Text)

type PBArg = Text

type FNFrameElement = Text


-- | Grammatical relation argument type (temporary type for "pseudo"-theta-role)
--
data GArg = GASBJ | GA1 | GA2
          deriving (Show,Eq,Ord,Generic)

instance Hashable GArg                   

-- | Grammatical relation
--
data GRel = GR_NP    (Maybe GArg)
          | GR_S     (Maybe GArg)
          | GR_SBAR  (Maybe GArg)
          | GR_PP    (Maybe Text)
          | GR_ADVP  (Maybe Text)
          | GR_ADJP
          | GR_X     Text
          deriving (Show,Eq,Ord,Generic)

instance Hashable GRel                   

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


instance (Hashable v, Hashable p) => Hashable (ArgPattern v p)


-----------------
-- For RoleMap --
-----------------


type RoleInstance = (SenseID,[(PBArg,FNFrameElement)])

type RolePattInstance v = (SenseID,[(ArgPattern v GRel,Int)])





