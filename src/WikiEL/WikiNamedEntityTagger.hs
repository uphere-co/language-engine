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
import qualified WikiEL.NamedEntity            as N
import qualified WikiEL.CoreNLP                as C
import qualified WikiEL.WikiEntityClass        as WEC


type NEClass = NamedEntityClass

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag


namedEntityAnnotator:: NameUIDTable -> [NamedEntityFrag] -> [(IRange, Vector ItemID)]
namedEntityAnnotator entities frags = reverse matchedItems
  where
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


data PreNE = UnresolvedUID NEClass             -- Tagged by CoreNLP NER, but no matched Wikidata UID
           | AmbiguousUID ([ItemID],NEClass)   -- Tagged by CoreNLP NER, and matched Wikidata UIDs of the NEClass
           | Resolved (ItemID, NEClass)        -- A wikidata UID of a CoreNLP NER Class type.
           | UnresolvedClass [ItemID]          -- Not tagged by CoreNLP NER, but matched Wikidata UID(s)
           deriving(Show, Eq, Generic)

instance ToJSON PreNE where
  toJSON = genericToJSON defaultOptions

instance FromJSON PreNE where
  parseJSON = genericParseJSON defaultOptions

isResolved :: PreNE -> Bool
isResolved (Resolved _ ) = True
isResolved _ = False

resolvedUID :: PreNE -> Either String ItemID
resolvedUID (Resolved (id,_)) = Right id
resolvedUID (UnresolvedUID _) = Left "Unresolved ItemID"
resolvedUID (AmbiguousUID _)  = Left "Ambiguous ItemID"
resolvedUID (UnresolvedClass _) = Left "Unresolved named entity class"

resolveNEClass :: WEC.WikiuidNETag -> NEClass -> Vector ItemID -> PreNE
resolveNEClass ts stag xs = g matchedUIDs
  where
    f accum uid | WEC.hasNETag ts (uid, stag)  = uid:accum
                | otherwise                    = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved (uid, stag)
    g uids  = AmbiguousUID (uids, stag)

resolveNEsImpl :: WEC.WikiuidNETag -> [(IRange,PreNE)] -> [(IRange, NEClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEsImpl ts accum [] [] = accum
resolveNEsImpl ts accum lhss@((lrange,ltag):ls) []  =
  resolveNEsImpl ts ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl ts accum [] rhss@((rrange,rtags):rs) =
  resolveNEsImpl ts ((rrange, UnresolvedClass (toList rtags)) : accum) [] rs
resolveNEsImpl ts accum lhss@((lrange,ltag):ls) rhss@((rrange,rtags):rs) =
  --case mergeDecision (relativePos lrange rrange) of
  case relativePos lrange rrange of
    Coincide  -> resolveNEsImpl ts ((lrange, resolveNEClass ts ltag rtags):accum) ls rs
    LinR      -> resolveNEsImpl ts (keepR:accum) lsIter rs
    RinL      -> resolveNEsImpl ts (keepL:accum) ls rsIter
    LbeforeR  -> resolveNEsImpl ts (keepL:accum) ls rhss
    RbeforeL  -> resolveNEsImpl ts (keepR:accum) lhss rs
    RoverlapL -> resolveNEsImpl ts (keepR:accum) ls rsIter
    LoverlapR -> resolveNEsImpl ts (keepL:accum) lsIter rs
  where
    keepL = (lrange, UnresolvedUID ltag)
    keepR = (rrange, UnresolvedClass (toList rtags))
    lsIter = untilNoOverlap (relativePos rrange . fst) ls
    rsIter = untilNoOverlap (relativePos lrange . fst) rs



resolveNEs :: WEC.WikiuidNETag -> [(IRange, NEClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEs ts lhss rhss = map (second assumeCorrectAnnotation) xs
  where
    xs = reverse (resolveNEsImpl ts [] lhss rhss)
    --TODO: correct otherClass
    assumeCorrectAnnotation (UnresolvedClass [itemID]) = Resolved (itemID, WEC.toNEClass WEC.otherClass)
    assumeCorrectAnnotation x = x

    

