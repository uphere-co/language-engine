{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Convert where

import           Data.Vector                           (Vector,toList)
import           Data.Text                             (Text)
import qualified Data.Text   as T
--
import WikiEL.EntityLinking
import WikiEL.Type                                     (EntityMention(..),EntityMentionUID(..),IRange(..),UIDCite(..))

getNameFromEntityMention :: EntityMention Text -> Text
getNameFromEntityMention x = case x of
                               Cite uid ref info@(ir,v,ne) -> T.intercalate " " (toList v)
                               Self uid     info@(ir,v,ne) -> T.intercalate " " (toList v)

getRangeFromEntityMention :: EntityMention Text -> (Int,Int)
getRangeFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (beg ir,end ir)
                                Self uid     info@(ir,v,ne) -> (beg ir, end ir)
    
getUIDFromEntityMention :: EntityMention Text -> Int
getUIDFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (_emuid uid)
                                Self uid     info@(ir,v,ne) -> (_emuid uid)

getNEFromEntityMention :: EntityMention Text -> Text
getNEFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (T.pack $ show ne)
                                Self uid     info@(ir,v,ne) -> (T.pack $ show ne)

