{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


{-|
  This module combines Wikidata tagger, WikiEL.WikiEntityTagger, and CoreNLP NER.
  by discarding possiblities that both results are inconsistent.
-}
module WikiEL.WikiNamedEntityTagger 
( module WikiEL.WikiNamedEntityTagger 
, WEC.mayCite
) where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')
import           Control.Arrow                         (second)
import qualified Data.Text                     as T
--
import           NLP.Type.NamedEntity                  (NamedEntityFrag,NamedEntityClass(Other),parseStr, _ftype,_fstr)
import           WikiEL.Type                           (EntityToken(..),IRange(..),NameUIDTable
                                                       ,PreNE(..),RelativePosition(..),WikiuidNETag)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Misc                           (relativePos, untilNoOverlap)
import           WikiEL.WikiEntityTagger               (wikiAnnotator)
import qualified WikiEL.WikiEntityClass        as WEC


parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> EntityToken (T.dropEnd 1 x) y) $ T.breakOnEnd (T.pack "/") tokenStr


parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)


parseStanfordNE :: EntityToken -> NamedEntityFrag
parseStanfordNE (EntityToken word tag) = parseStr word tag


loadStanfordNERoutput :: Text -> [NamedEntityFrag]
loadStanfordNERoutput content = map parseStanfordNE (parseNEROutputStr content)


getWords :: [NamedEntityFrag] -> [Text]
getWords  = map _fstr


getNETags :: [NamedEntityFrag] -> [NamedEntityClass]
getNETags = map _ftype


namedEntityAnnotator:: NameUIDTable -> [NamedEntityFrag] -> [(IRange, Vector ItemID)]
namedEntityAnnotator entities frags = reverse matchedItems
  where
    ws = map _fstr frags
    matchedItems = wikiAnnotator entities ws


partitonFrags:: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
partitonFrags frags = ifoldr f [] (fromList frags)
  where
    decL (IRange beg end) = IRange (beg-1) end
    toRange idx = IRange idx (idx+1)
    tagType = _ftype
    g idx frag = (toRange idx, tagType frag)
    f idx frag [] = [g idx frag]
    f idx frag accum@((range, tag):ss) | tagType frag == tag = (decL range, tag):ss
                                       | otherwise            = g idx frag : accum

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
getStanfordNEs = dropNonNE . partitonFrags
  where
    dropNonNE = filter (\x-> snd x /= Other)

uidCandidates :: PreNE -> [ItemID]
uidCandidates (UnresolvedUID _)      = []
uidCandidates (AmbiguousUID (ids,_)) = ids
uidCandidates (Resolved     (i,_))  = [i]
uidCandidates (UnresolvedClass ids)  = ids
uidCandidates (OnlyTextMatched i _)    = [i]

isResolved :: PreNE -> Bool
isResolved (Resolved _ ) = True
isResolved _ = False

resolvedUID :: PreNE -> Either String ItemID
resolvedUID (Resolved (i,_))     = Right i
resolvedUID (UnresolvedUID _)     = Left "Unresolved ItemID"
resolvedUID (AmbiguousUID _)      = Left "Ambiguous ItemID"
resolvedUID (UnresolvedClass _)   = Left "Unresolved named entity class"
resolvedUID (OnlyTextMatched i _) = Right i

resolveNEClass :: WikiuidNETag -> NamedEntityClass -> Vector ItemID -> PreNE
resolveNEClass ts stag xs = g matchedUIDs
  where
    u = WEC.guessItemClass2 ts stag
    f !accum uid | WEC.mayCite stag (u uid)  = uid:accum
                 | otherwise             = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved     (uid, u uid)
    g []    = UnresolvedUID stag
    g uids  = AmbiguousUID (uids, stag)

resolveNEsImpl :: WikiuidNETag -> [(IRange,PreNE)] -> [(IRange, NamedEntityClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEsImpl _ts accum [] [] = accum
resolveNEsImpl ts accum ((lrange,ltag):ls) []  =
  resolveNEsImpl ts ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl ts accum [] ((rrange,rtags):rs) =
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



resolveNEs :: WikiuidNETag -> [(IRange, NamedEntityClass)] -> [(IRange, Vector ItemID)] -> [(IRange,PreNE)]
resolveNEs ts lhss rhss = map (second assumeCorrectAnnotation) xs
  where
    xs = reverse (resolveNEsImpl ts [] lhss rhss)
    assumeCorrectAnnotation (UnresolvedClass [itemID]) = Resolved (itemID, WEC.guessItemClass ts itemID)
    assumeCorrectAnnotation x = x

    

