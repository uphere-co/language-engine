{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^.),(^?),(.~),(&),_Just)
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
import           NLP.Syntax.Type.XBar    (Zipper,NounP,DetP,CompVP(..),AdjunctVP(..)
                                         ,Prep(..),PrepClass(..),PP,_CompPP_DP
                                         ,TaggedLemma(..),pennTree,tagList
                                         ,complement,headX,maximalProjection
                                         ,mkPP,hp_prep,hp_pclass,hn_range)


hasEmptyPreposition :: TaggedLemma t -> Range -> Bool
hasEmptyPreposition tagged rng =
  fromMaybe False $ do
    z <- find (isChunkAs NP . current) (extractZipperByRange rng (tagged^.pennTree))
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
  guard $ fromMaybe False $ do
            let rng = dp^.maximalProjection
            find (isMatchedTime rng) (tagged^.tagList)
            return True
  (return . CompVP_PP . (headX.hp_prep .~ pp^.headX.hp_prep) . (headX.hp_pclass .~ PC_Time)) pp



identifyInternalTimePrep :: TaggedLemma t
                         -> DetP t
                         -> (DetP t,[AdjunctVP t])
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
  (b_h,e_h) <- dp^?complement._Just.headX.hn_range
  let rng_head = if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
      dp' = dp & (maximalProjection .~ rng_dp') . (complement._Just.headX.hn_range .~ rng_head) . (complement._Just.maximalProjection .~ rng_dp')
  return (dp',[AdjunctVP_Unresolved z_tpp])


--
-- | This is the list of English prepositions.
--   Main source: https://en.wikipedia.org/wiki/List_of_English_prepositions
--   I commented out (law,archaic,poetic,abbreviation,rare,formal,hyphenated,colonated)
--   such as aboard, absent, cross, apropos, apud, chez, pace, post, pre, pro, qua, re, sans, vice, vis-a-vis
--
singleWordPrepositions :: [(Text,[Text])]                      -- FrameNet Frames
singleWordPrepositions = [ ("about"     , ["Topic", "Proportional_quantity"])
                         , ("above"     , ["Directional_locative_relation"])
                         , ("across"    , ["Distributed_position"])
                         , ("after"     , [ "Time_vector", "Relative_time"])
                         , ("against"   , [ "Taking_sides", "Spatial_contact" ])
                         , ("along"     , [ "Locative_relation", "Non-gradable_proximity" ])
                         , ("alongside" , [ ])
                         , ("amid"      , [ "Interior_profile_relation" ])
                         , ("amidst"    , [ "Contrary_circumstances" ])
                         , ("among"     , [ "Be_subset_of", "Interior_profile_relation" ])
                         , ("around"    , [ "Temporal_collocation", "Proportional_quantity", "Surrounding", "Distributed_position" ])
                         , ("as"        , [ "Performers_and_roles" ])
                         , ("astride"   , [ "Locative_relation", "Surrounding" ])
                         , ("at"        , [ "Locative_relation", "Temporal_collocation", "Being_employed", "Spatial_co-location" ])
                         , ("on"        , [ "Topic", "Being_in_operation", "Temporal_collocation", "Accuracy", "Means", "Spatial_contact", "Non-gradable_proximity" ])
                         , ("atop"      , [ "Spatial_contact" ])
                         , ("ontop"     , [ ])
                         , ("bar"       , [ ])
                         , ("before"    , [ "Time_vector" ])
                         , ("behind"    , [ "Responsibility", "Non-gradable_proximity" ])
                         , ("below"     , [ "Directional_locative_relation" ])
                         , ("beneath"   , [ "Non-gradable_proximity" ])
                         , ("beside"    , [ "Non-gradable_proximity" ])
                         , ("besides"   , [ "Non-gradable_proximity" ])
                         , ("between"   , [ "Interior_profile_relation" ])
                         , ("beyond"    , [ "Locative_relation" ])
                         , ("but"       , [ ])
                         , ("by"        , [ "Means", "Non-gradable_proximity" ])
                         , ("circa"     , [ "Proportional_quantity" ])
                         , ("come"      , [ ])
                         , ("despite"   , [ "Concessive", "Contrary_circumstances" ])
                         , ("down"      , [ "Locative_relation", "Change_position_on_a_scale" ])
                         , ("during"    , [ "Temporal_collocation" ])
                         , ("except"    , [ ])
                         , ("for"       , [ "Taking_sides", "Duration_relation" ])
                         , ("from"      , [ "Time_vector", "Origin", "Evidence" ])
                         , ("in"        , [ "Taking_time", "Interior_profile_relation", "Wearing", "Fields", "Expected_location_of_person", "Temporal_collocation", "Medium" ])
                         , ("inside"    , [ "Interior_profile_relation" ])
                         , ("into"      , [ "Goal" ])
                         , ("less"      , [ ])
                         , ("like"      , [ "Similarity" ])
                         , ("minus"     , [ "Non-commutative_statement" ])
                         , ("near"      , [ "Locative_relation" ])
                         , ("notwithstanding"          , [ ])
                         , ("of"        , [ "Partitive", "Origin", "Age" ])
                         , ("off"       , [ "Being_in_operation", "Accuracy", "Spatial_contact" ])
                         , ("on"        , [ "Being_in_operation", "Topic", "Temporal_collocation", "Accuracy", "Means", "Spatial_contact", "Non-gradable_proximity" ])
                         , ("onto"      , [ ])
                         , ("opposite"  , [ "Non-gradable_proximity" ])
                         , ("out"       , [ "Expected_location_of_person", "Locative_relation" ])
                         , ("outside"   , [ "Interior_profile_relation" ])
                         , ("over"      , [ "Locative_relation", "Proportional_quantity", "Non-gradable_proximity", "Distributed_position", "Temporal_collocation" ])
                         , ("past"      , [ "Locative_relation" ])
                         , ("per"       , [ ])
                         , ("save"      , [ ])
                         , ("short"     , [ ])
                         , ("since"     , [ "Time_vector" ])
                         , ("than"      , [ ])
                         , ("through"   , [ "Time_vector" ])
                         , ("througout" , [ "Locative_relation" ])
                         , ("to"        , [ "Goal", "Locative_relation" ])
                         , ("toward"    , [ ])
                         , ("towards"   , [ ])
                         , ("under"     , [ "Non-gradable_proximity","Proportional_quantity" ])
                         , ("underneath", [ "Non-gradable_proximity" ])
                         , ("unlike"    , [ "Similarity" ])
                         , ("until"     , [ "Time_vector" ])
                         , ("till"      , [ "Time_vector" ])
                         , ("up"        , [ "Locative_relation" ])
                         , ("upon"      , [ "Spatial_contact" ])
                         , ("upside"    , [ ])
                         , ("versus"    , [ ])
                         , ("via"       , [ ])
                         , ("with"      , [ "Accompaniment", "Have_associated" ])
                         , ("within"    , [ "Within_distance", "Temporal_collocation", "Interior_profile_relation" ])
                         , ("without"   , [ "Negation" ])
                         , ("worth"     , [ ])
                         ]


multiWordPrepositions :: [Text]
multiWordPrepositions = [ -- two words
                          "according to"
                        , "adjacent to"
                        , "ahead of"
                        , "along with"                    -- Accompaniment
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
                        , "in point of"
                        , "in spite of"
                        , "on account of"
                        , "on behalf of"
                        , "on top of"
                        , "with regard to"
                        , "with respect to"
                        , "with a view to"
                        ]
