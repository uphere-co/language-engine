{-# LANGUAGE TemplateHaskell #-}

module VerbNet.Type.SemLink where

import           Control.Lens
import           Data.Text


--------------------------
-- VerbNet <-> FrameNet --
--------------------------

data VNFN = VNFN { _vnc_class :: Text
                 , _vnc_vnmember :: Text
                 , _vnc_fnframe :: Text
                 , _vnc_fnlexent :: Text
                 , _vnc_versionID :: Text
                 }
          deriving Show

makeLenses ''VNFN


data VNFNMap = VNFNMap { _vnfnmap_vnfns :: [VNFN]
                       , _vnfnmap_date :: Text
                       }
             deriving Show

makeLenses ''VNFNMap


---------------------------------------
-- VerbNet <-> FrameNet Role Mapping --
---------------------------------------

data VNFNRole = VNFNRole { _vnfnrole_fnrole :: Text
                         , _vnfnrole_vnrole :: Text
                         }
              deriving Show

makeLenses ''VNFNRole


data VNFNRoleInstance = VNFNRoleInstance { _vnfnroleinst_class   :: Text
                                         , _vnfnroleinst_fnframe :: Text
                                         , _vnfnroleinst_roles   :: [VNFNRole]
                                         }
                      deriving Show

makeLenses ''VNFNRoleInstance


data VNFNRoleMap = VNFNRoleMap { _vnfnrolemap_vnfnroles :: [VNFNRoleInstance]
                               , _vnfnrolemap_date      :: Text
                               }
                 deriving Show

makeLenses ''VNFNRoleMap



--------------------------
-- PropBank <-> VerbNet --
--------------------------

data PBVNRole = PBVNRole { _pbvnrole_pbarg   :: Text
                         , _pbvnrole_vntheta :: Text
                         }
              deriving Show

makeLenses ''PBVNRole


data PBVNArgMap = PBVNArgMap { _pbvnarg_pbroleset :: Text
                             , _pbvnarg_vnclass   :: Text
                             , _pbvnarg_roles     :: [PBVNRole]
                             }
                deriving Show

makeLenses ''PBVNArgMap


data PBVN = PBVN { _pbvn_lemma :: Text
                 , _pbvn_argmap :: [PBVNArgMap]
                 }
          deriving Show

makeLenses ''PBVN


data PBVNMap = PBVNMap { _pbvnmap_predicates :: [PBVN] }
             deriving Show

makeLenses ''PBVNMap
