{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^.),(^?),(.~),(&))
import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
--
import           Data.BitreeZipper       (current,parent,extractZipperByRange)
import           Data.Range              (isInsideR,Range)
import           NLP.Type.PennTreebankII (ChunkTag(..),getRange)
import           NLP.Type.TagPos         (TagPos(..),TokIdx)
--
import           NLP.Syntax.Util         (beginEndToRange,isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DetP,CompVP(..)
                                         ,Prep(..),PrepClass(..),PP,_CompPP_DP
                                         ,TaggedLemma(..),pennTree,tagList
                                         ,complement,headX,maximalProjection
                                         ,mkPP)


hasEmptyPreposition :: TaggedLemma t -> Range -> Bool
hasEmptyPreposition tagged rng =
  fromMaybe False $ do
    z <- find (isChunkAs NP . current) (extractZipperByRange rng (tagged^.pennTree))
    -- guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True


isMatchedTime :: Range -> TagPos TokIdx MarkType -> Bool
isMatchedTime rng (TagPos (b,e,t)) = beginEndToRange (b,e) == rng && t == MarkTime


checkEmptyPrep :: TaggedLemma t -> DetP t -> CompVP t
checkEmptyPrep tagged dp =
  let rng = dp^.maximalProjection
      r = fromMaybe False $ do
            -- check bare noun adverb
            find (isMatchedTime rng) (tagged^.tagList)
            return (hasEmptyPreposition tagged rng)
  in if r
     then CompVP_PP (mkPP (Prep_NULL,PC_Time) rng dp)
     else CompVP_DP dp


checkTimePrep :: TaggedLemma t -> PP t -> CompVP t
checkTimePrep tagged pp = fromMaybe (CompVP_PP pp) $ do
  dp <- pp^? complement . _CompPP_DP
  let (prep,_pclass) = pp^.headX
  guard $ fromMaybe False $ do
            let rng = dp^.maximalProjection
            find (isMatchedTime rng) (tagged^.tagList)
            return True
  return (CompVP_PP ((headX .~ (prep,PC_Time)) pp))



identifyInternalTimePrep :: TaggedLemma t
                         -> DetP t
                         -> (DetP t,[Zipper t])
identifyInternalTimePrep tagged dp = fromMaybe (dp,[]) $ do
  let rng_dp@(b_dp,_e_dp) = dp^.maximalProjection
  TagPos (b0,e0,_)
    <- find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) `isInsideR` rng_dp && t == MarkTime) (tagged^.tagList)
  let rng_time = beginEndToRange (b0,e0)
  z_tdp <- find (isChunkAs NP . current) (extractZipperByRange rng_time (tagged^.pennTree))
  z_tpp <- parent z_tdp
  guard (isChunkAs PP (current z_tpp))
  let (b_tpp,_e_tpp) = getRange (current z_tpp)
      rng_dp' = (b_dp,b_tpp-1)
      rng_head = let (b_h,e_h) = dp^.headX
                 in if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
      dp' = dp & (headX .~ rng_head) . (maximalProjection .~ rng_dp')
  return (dp',[z_tpp])


--
-- | This is the list of English prepositions.
--   source: https://en.wikipedia.org/wiki/List_of_English_prepositions
--   I commented out (law,archaic,poetic,abbreviation,rare,formal,hyphenated,colonated)
--
singleWordPrepositions :: [Text]
singleWordPrepositions = [ "aboard"
                         , "about"
                         , "above"
                         -- , "absent"
                         , "across"
                         -- , "cross"
                         , "after"
                         , "against"
                         , "along"
                         , "alongside"
                         , "amid"
                         , "among"
                         -- , "apropos"
                         -- , "apud"
                         , "around"
                         , "as"
                         , "astride"
                         , "at"
                         , "on"
                         , "atop"
                         , "ontop"
                         , "bar"
                         , "before"
                         , "behind"
                         , "below"
                         , "beneath"
                         , "beside"
                         , "besides"
                         , "between"
                         , "beyond"
                         , "but"
                         , "by"
                         -- , "chez"
                         , "circa"
                         , "come"
                         , "despite"
                         , "down"
                         , "during"
                         , "except"
                         , "for"
                         , "from"
                         , "in"
                         , "inside"
                         , "into"
                         , "less"
                         , "like"
                         , "minus"
                         , "near"
                         , "notwithstanding"
                         , "of"
                         , "off"
                         , "on"
                         , "onto"
                         , "opposite"
                         , "out"
                         , "outside"
                         , "over"
                         -- , "pace"
                         , "past"
                         , "per"
                         -- , "post"
                         -- , "pre"
                         -- , "pro"
                         -- , "qua"
                         -- , "re"
                         -- , "sans"
                         , "save"
                         , "short"
                         , "since"
                         , "than"
                         , "through"
                         , "througout"
                         , "to"
                         , "toward"
                         , "towards"
                         , "under"
                         , "underneath"
                         , "unlike"
                         , "until"
                         , "till"
                         , "up"
                         , "upon"
                         , "upside"
                         , "versus"
                         , "via"
                         -- , "vice"
                         -- , "vis-a-vis"
                         , "with"
                         , "within"
                         , "without"
                         , "worth"
                         ]


multiWordPrepositions :: [Text]
multiWordPrepositions = [ -- two words
                          "according to"
                        , "adjacent to"
                        , "ahead of"
                        , "apart from"
                        , "as for"
                        , "as of"
                        , "as per"
                        , "as regards"
                        , "aside from"
                        , "back to"
                        , "because of"
                        , "close to"
                        , "due to"
                        , "except for"
                        , "far from"
                        , "inside of"
                        , "instead of"
                        , "left of"
                        , "near to"
                        , "next to"
                        , "opposite of"
                        , "opposite to"
                        , "out from"
                        , "out of"
                        , "outside of"
                        , "owing to"
                        , "prior to"
                        , "pursuant to"
                        , "rather than"
                        , "regardless of"
                        , "right of"
                        , "subsequent to"
                        , "such as"
                        , "thanks to"
                        , "up to"
                          -- three words
                        , "as far as"
                        , "as opposed to"
                        , "as soon as"
                        , "as well as"
                          -- more words
                        , "at the behest of"
                        , "by means of"
                        , "by virtue of"
                        , "for the sake of"
                        , "in accordance with"
                        , "in addition to"
                        , "in case of"
                        , "in front of"
                        , "in lieu of"
                        , "in place of"
                        , "in popint of"
                        , "in spite of"
                        , "on account of"
                        , "on behalf of"
                        , "on top of"
                        , "with regard to"
                        , "with respect to"
                        , "with a view to"
                        ]
