module WikiEL.Convert where

import           Data.Vector                           (Vector)
--
import WikiEL.EntityLinking
import WikiEL.Misc                                     (IRange(..))

getNameFromEntityMention :: (Show w) => (EntityMention w) -> Vector w
getNameFromEntityMention x = case x of
                               Cite uid ref info@(ir,v,ne) -> v
                               Self uid     info@(ir,v,ne) -> v

getRangeFromEntityMention :: (Show w) => (EntityMention w) -> (Int,Int)
getRangeFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (beg ir,end ir)
                                Self uid     info@(ir,v,ne) -> (beg ir, end ir)
    
getUIDFromEntityMention :: (Show w) => (EntityMention w) -> Int
getUIDFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (_emuid uid)
                                Self uid     info@(ir,v,ne) -> (_emuid uid)

getNEFromEntityMention :: (Show w) => (EntityMention w) -> String
getNEFromEntityMention x = case x of
                                Cite uid ref info@(ir,v,ne) -> (show ne)
                                Self uid     info@(ir,v,ne) -> (show ne)

