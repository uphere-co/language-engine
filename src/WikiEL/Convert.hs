module WikiEL.Convert where

import           Data.Vector                           (Vector)
--
import WikiEL.EntityLinking

getNameFromEntityMention :: (Show w) => (EntityMention w) -> IO (Vector w)
getNameFromEntityMention x = do
  case x of
    Cite uid ref info@(ir,v,ne) -> return v
    Self uid info@(ir,v,ne)   -> return v
