{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module WikiEL.WikiNamedEntityTagger where

import           Data.Aeson
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')
import           Control.Arrow                         (second)
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           GHC.Generics                          (Generic)

import           NLP.Type.NamedEntity                  (NamedEntity,NamedEntityFrag,NamedEntityClass(Other),parseStr, _ftype,_fstr)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Misc                           (IRange(..),RelativePosition(..), relativePos, untilNoOverlap)
import           WikiEL.WikiEntityTagger               (NameUIDTable,buildEntityTable,wikiAnnotator)
import           WikiEL.WikiEntityClass                (WikiUID2NETag,ItemClass,neClassMatch,getNEClass,toNEClass)
import qualified WikiEL.NamedEntity            as N
import qualified WikiEL.CoreNLP                as C


type NEClass = NamedEntityClass

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag


namedEntityAnnotator:: NameUIDTable -> WikiUID2NETag -> [NamedEntityFrag] -> [(IRange, Vector (ItemID, ItemClass))]
namedEntityAnnotator entities uidTypes frags = reverse (map (second (V.map f)) matchedItems)
  where
    f uid= (uid, getNEClass uidTypes uid)
    words = map _fstr frags
    matchedItems = wikiAnnotator entities words

partitonFrags:: [NamedEntityFrag] -> [(IRange, NEClass)]
partitonFrags frags = ifoldr f [] (fromList frags)
  where
    decL (IRange beg end) = IRange (beg-1) end
    toRange idx = IRange idx (idx+1)
    tagType = _ftype
    g idx frag = (toRange idx, tagType frag)
    f idx frag [] = [g idx frag]
    f idx frag accum@((range, tag):ss) | tagType frag == tag = (decL range, tag):ss
                                       | otherwise            = g idx frag : accum

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NEClass)]
getStanfordNEs = dropNonNE . partitonFrags
  where
    dropNonNE = filter (\x-> snd x /= Other)


data PreNE = UnresolvedUID NEClass
           | AmbiguousUID [ItemID]
           | Resolved (ItemID, NEClass)
           | UnresolvedClass [(ItemID, ItemClass)]
           deriving(Show, Eq, Generic)

instance ToJSON PreNE where
  toJSON = genericToJSON defaultOptions

resolvedUID :: PreNE -> Either String ItemID
resolvedUID (Resolved (id,_)) = Right id
resolvedUID (UnresolvedUID _) = Left "Unresolved ItemID"
resolvedUID (AmbiguousUID _)  = Left "Ambiguous ItemID"
resolvedUID (UnresolvedClass _) = Left "Unresolved named entity class"

resolveNEClass :: NEClass -> Vector (ItemID, ItemClass) -> PreNE
resolveNEClass stag xs = g matchedUIDs
  where
    f accum (uid,tag) | neClassMatch stag tag = uid:accum
                      | otherwise             = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved (uid, stag)
    g uids  = AmbiguousUID uids

resolveNEsImpl :: [(IRange,PreNE)] -> [(IRange, NEClass)] -> [(IRange, Vector (ItemID, ItemClass))] -> [(IRange,PreNE)]
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



resolveNEs :: [(IRange, NEClass)] -> [(IRange, Vector (ItemID, ItemClass))] -> [(IRange,PreNE)]
resolveNEs lhss rhss = map (second assumeCorrectAnnotation) xs
  where
    xs = reverse (resolveNEsImpl [] lhss rhss)
    assumeCorrectAnnotation (UnresolvedClass [(itemID, itemClass)]) = Resolved (itemID, toNEClass itemClass)
    assumeCorrectAnnotation x = x

    

