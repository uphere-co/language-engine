{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiNamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')
import           Control.Arrow                         (second)
import           Misc                                  (IRange(..),RelativePosition(..), relativePos, untilNoOverlap)
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (NameUIDTable,buildEntityTable,wikiAnnotator)
import           WikiEntityClass                       (WikiUID2NETag,getNEClass)
import           NamedEntity                           (NamedEntity,NamedEntityFrag,NamedEntityClass,parseStr)
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified WikiEntity                    as Wiki
import qualified NamedEntity                   as N
import qualified CoreNLP                       as C


type NEClass = NamedEntityClass

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag


namedEntityAnnotator:: NameUIDTable -> WikiUID2NETag -> [NamedEntityFrag] -> [(IRange, Vector (Wiki.UID, NEClass))]
namedEntityAnnotator entities uidTypes frags = reverse (map (second (V.map f)) matchedItems)
  where
    f uid= (uid, getNEClass uidTypes uid)
    words = map N._fstr frags
    matchedItems = wikiAnnotator entities words

partitonFrags:: [NamedEntityFrag] -> [(IRange, NEClass)]
partitonFrags frags = ifoldr f [] (fromList frags)
  where
    decL (IRange beg end) = IRange (beg-1) end
    toRange idx = IRange idx (idx+1)
    tagType = N._ftype
    g idx frag = (toRange idx, tagType frag)
    f idx frag [] = [g idx frag]
    f idx frag accum@((range, tag):ss) | tagType frag == tag = (decL range, tag):ss
                                       | otherwise            = g idx frag : accum

dropNonNE:: [(IRange, NEClass)] -> [(IRange, NEClass)]
dropNonNE = filter (\x-> snd x /= N.Other)

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NEClass)]
getStanfordNEs = dropNonNE . partitonFrags

buildTagUIDTable :: NEClass -> Vector Wiki.UID -> Vector (Wiki.UID, NEClass)
buildTagUIDTable tag = V.map (\uid -> (uid,tag)) 


data PreNE = UnresolvedUID NEClass
           | AmbiguousUID [Wiki.UID]
           | Resolved (Wiki.UID, NEClass)
           | UnresolvedClass [(Wiki.UID, NEClass)]
           deriving(Show, Eq)
                
resolveNEClass :: NEClass -> Vector (Wiki.UID, NEClass) -> PreNE
resolveNEClass stag xs = g matchedUIDs
  where
    f accum (uid,tag) | tag==stag = uid:accum
                      | otherwise = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved (uid, stag)
    g uids  = AmbiguousUID uids

resolveNEsImpl :: [(IRange,PreNE)] -> [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEsImpl accum [] [] = accum
resolveNEsImpl accum lhss@((lrange,ltag):ls) []  =
  resolveNEsImpl ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl accum [] rhss@((rrange,rtags):rs) =
  resolveNEsImpl ((rrange, UnresolvedClass (toList rtags)) : accum) [] rs
resolveNEsImpl accum lhss@((lrange,ltag):ls) rhss@((rrange,rtags):rs) =
  --case mergeDecision (relativePos lrange rrange) of
  case relativePos lrange rrange of
    Coincide  -> resolveNEsImpl ((lrange, resolveNEClass ltag rtags):accum) ls rs
    LinR      -> resolveNEsImpl (keepR:accum) lsIter rs
    RinL      -> resolveNEsImpl (keepL:accum) ls rsIter
    LbeforeR  -> resolveNEsImpl (keepL:accum) ls rhss
    RbeforeL  -> resolveNEsImpl (keepR:accum) lhss rs
    RoverlapL -> resolveNEsImpl (keepR:accum) ls rsIter
    LoverlapR -> resolveNEsImpl (keepL:accum) lsIter rs
  where
    keepL = (lrange, UnresolvedUID ltag)
    keepR = (rrange, UnresolvedClass (toList rtags))
    lsIter = untilNoOverlap (relativePos rrange . fst) ls
    rsIter = untilNoOverlap (relativePos lrange . fst) rs

resolveNEs :: [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEs lhss rhss = reverse (resolveNEsImpl [] lhss rhss)

