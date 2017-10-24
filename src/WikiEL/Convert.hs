{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Convert where

import           Control.Lens
import           Data.Vector       (Vector,toList)
import           Data.Text         (Text)
import qualified Data.Text    as T
--
import WikiEL.EntityLinking
import WikiEL.Type                                     (EntityMention(..),EntityMentionUID(..),IRange(..),UIDCite(..)
                                                       ,beg,end)

getNameFromEntityMention :: EntityMention Text -> Text
getNameFromEntityMention x = case x of
                               Cite uid ref info@(ir,v,ne) -> T.intercalate " " (toList v)
                               Self uid     info@(ir,v,ne) -> T.intercalate " " (toList v)

getRangeFromEntityMention :: EntityMention Text -> (Int,Int)
getRangeFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (ir ^. beg,ir ^. end)
                                Self uid     info@(ir,v,ne) -> (ir ^. beg,ir ^. end)
    
getUIDFromEntityMention :: EntityMention Text -> Int
getUIDFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (_emuid uid)
                                Self uid     info@(ir,v,ne) -> (_emuid uid)

getNEFromEntityMention :: EntityMention Text -> Text
getNEFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (T.pack $ show ne)
                                Self uid     info@(ir,v,ne) -> (T.pack $ show ne)

