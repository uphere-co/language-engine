{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Convert where

import           Control.Lens
import           Data.Vector       (toList)
import           Data.Text         (Text)
import qualified Data.Text    as T
--
import WikiEL.Type                                     (EntityMention,EntityMentionUID(..),UIDCite(..)
                                                       ,beg,end)

getNameFromEntityMention :: EntityMention Text -> Text
getNameFromEntityMention x = case x of
                               Cite _uid _ref _info@(_ir,v,_ne) -> T.intercalate " " (toList v)
                               Self _uid      _info@(_ir,v,_ne) -> T.intercalate " " (toList v)

getRangeFromEntityMention :: EntityMention Text -> (Int,Int)
getRangeFromEntityMention x = case x of
                                Cite _uid _ref _info@(ir,_v,_ne) -> (ir ^. beg,ir ^. end)
                                Self _uid      _info@(ir,_v,_ne) -> (ir ^. beg,ir ^. end)
    
getUIDFromEntityMention :: EntityMention Text -> Int
getUIDFromEntityMention x = case x of
                                Cite uid _ref _info@(_ir,_v,_ne) -> (_emuid uid)
                                Self uid      _info@(_ir,_v,_ne) -> (_emuid uid)

getNEFromEntityMention :: EntityMention Text -> Text
getNEFromEntityMention x = case x of
                                Cite _uid _ref _info@(_ir,_v,ne) -> (T.pack $ show ne)
                                Self _uid      _info@(_ir,_v,ne) -> (T.pack $ show ne)

