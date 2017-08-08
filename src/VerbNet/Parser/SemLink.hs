{-# LANGUAGE OverloadedStrings #-}

module VerbNet.Parser.SemLink where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath
--
import           Text.Taggy.Lens.Util
--
import           VerbNet.Type.SemLink


--------------------------
-- VerbNet <-> FrameNet --
--------------------------

p_vnfn :: Element -> Parser VNFN
p_vnfn x = VNFN <$> x .: "class"
                <*> x .: "vnmember"
                <*> x .: "fnframe"
                <*> x .: "fnlexent"
                <*> x .: "versionID"


p_vnfnmap :: Element -> Parser VNFNMap
p_vnfnmap x = VNFNMap <$> traverse p_vnfn (getOnly x "vncls")
                      <*> x .: "date"


---------------------------------------
-- VerbNet <-> FrameNet Role Mapping --
---------------------------------------

p_vnfnrole :: Element -> Parser VNFNRole
p_vnfnrole x = VNFNRole <$> x .: "fnrole" <*> x .: "vnrole"


p_vnfnroleinst :: Element -> Parser VNFNRoleInstance
p_vnfnroleinst x = VNFNRoleInstance <$> x .: "class"
                                    <*> x .: "fnframe"
                                    <*> p_list p_vnfnrole "role" "roles" x


p_vnfnrolemap :: Element -> Parser VNFNRoleMap
p_vnfnrolemap x = VNFNRoleMap <$> traverse p_vnfnroleinst (getOnly x "vncls")
                              <*> x .: "date"

--------------------------
-- VerbNet <-> PropBank --
--------------------------

p_pbvn_role :: Element -> Parser PBVNRole
p_pbvn_role x = PBVNRole <$> x .: "pb-arg"
                         <*> x .: "vn-theta"
                         

p_pbvn_argmap :: Element -> Parser PBVNArgMap
p_pbvn_argmap x = PBVNArgMap <$> x .: "pb-roleset"
                             <*> x .: "vn-class"
                             <*> traverse p_pbvn_role (getOnly x "role")


p_pbvn :: Element -> Parser PBVN
p_pbvn x = PBVN <$> x .: "lemma"
                <*> (p_pbvn_argmap =<< getOnly1 x "argmap")


p_pbvnmap :: Element -> Parser PBVNMap
p_pbvnmap x = PBVNMap <$> traverse p_pbvn (getOnly x "predicate")

