{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Text
--
import Lexicon.Query            (loadRoleInsts)
import Lexicon.Type
import NLP.Syntax.Type.Verb
import NLP.Type.PennTreebankII
import NLP.Type.SyntaxProperty
--
import SRL.Analyze.ARB
import SRL.Analyze.Format
import SRL.Analyze.Type

{-
mg1 =
  ( "Donald Trump seeks to open U.S. Arctic waters to drilling."
  , MeaningGraph {_mg_vertices = [MGPredicate {_mv_id = 1, _mv_range = (0,11), _mv_frame = "Seeking", _mv_verb = VerbProperty {_vp_index = 2, _vp_lemma = Lemma {unLemma = "seek"}, _vp_tense = Present, _vp_aspect = Simple, _vp_voice = Active, _vp_auxiliary = [], _vp_negation = Nothing, _vp_words = [("seek",(2,Lemma {unLemma = "seek"}))]}},MGPredicate {_mv_id = 2, _mv_range = (3,10), _mv_frame = "Activity_start", _mv_verb = VerbProperty {_vp_index = 4, _vp_lemma = Lemma {unLemma = "open"}, _vp_tense = Present, _vp_aspect = Simple, _vp_voice = Active, _vp_auxiliary = [("to",(3,Lemma {unLemma = "to"}))], _vp_negation = Nothing, _vp_words = [("to",(3,Lemma {unLemma = "to"})),("open",(4,Lemma {unLemma = "open"}))]}},MGEntity {_mv_id = 3, _mv_range = (0,1), _mv_text = "Donald Trump", _mv_resolved_entities = ["Resolved (Q22686,Class:\"Person\")"]},MGEntity {_mv_id = 4, _mv_range = (6,10), _mv_text = "U.S. Arctic waters to drilling", _mv_resolved_entities = ["Resolved (Q386786,Class:\"Organization\")"]}], _mg_edges = [MGEdge {_me_relation = "Cognizer_agent", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 3},MGEdge {_me_relation = "Sought_entity", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 2},MGEdge {_me_relation = "Agent", _me_ismodifier = False, _me_prep = Nothing, _me_start = 2, _me_end = 3},MGEdge {_me_relation = "Activity", _me_ismodifier = False, _me_prep = Nothing, _me_start = 2, _me_end = 4}]}
  )
  
mg2 =
  ( "After relying on oil to fuel its economy for more than half a century, Saudi Arabia is turning to its other abundant natural resource to take it beyond the oil age -- desert."
  , MeaningGraph {_mg_vertices = [MGPredicate {_mv_id = 1, _mv_range = (1,13), _mv_frame = "Reliance", _mv_verb = VerbProperty {_vp_index = 1, _vp_lemma = Lemma {unLemma = "rely"}, _vp_tense = Present, _vp_aspect = Progressive, _vp_voice = Active, _vp_auxiliary = [], _vp_negation = Nothing, _vp_words = [("rely",(1,Lemma {unLemma = "rely"}))]}},MGPredicate {_mv_id = 2, _mv_range = (4,13), _mv_frame = "Cause_to_start", _mv_verb = VerbProperty {_vp_index = 5, _vp_lemma = Lemma {unLemma = "fuel"}, _vp_tense = Present, _vp_aspect = Simple, _vp_voice = Active, _vp_auxiliary = [("to",(4,Lemma {unLemma = "to"}))], _vp_negation = Nothing, _vp_words = [("to",(4,Lemma {unLemma = "to"})),("fuel",(5,Lemma {unLemma = "fuel"}))]}},MGPredicate {_mv_id = 3, _mv_range = (0,34), _mv_frame = "Change_direction", _mv_verb = VerbProperty {_vp_index = 18, _vp_lemma = Lemma {unLemma = "turn"}, _vp_tense = Present, _vp_aspect = Progressive, _vp_voice = Active, _vp_auxiliary = [], _vp_negation = Nothing, _vp_words = [("be",(17,Lemma {unLemma = "be"})),("turn",(18,Lemma {unLemma = "turn"}))]}},MGPredicate {_mv_id = 4, _mv_range = (25,33), _mv_frame = "Taking", _mv_verb = VerbProperty {_vp_index = 26, _vp_lemma = Lemma {unLemma = "take"}, _vp_tense = Present, _vp_aspect = Simple, _vp_voice = Active, _vp_auxiliary = [("to",(25,Lemma {unLemma = "to"}))], _vp_negation = Nothing, _vp_words = [("to",(25,Lemma {unLemma = "to"})),("take",(26,Lemma {unLemma = "take"}))]}},MGEntity {_mv_id = 5, _mv_range = (6,7), _mv_text = "its economy", _mv_resolved_entities = []},MGEntity {_mv_id = 6, _mv_range = (15,16), _mv_text = "Saudi Arabia", _mv_resolved_entities = []},MGEntity {_mv_id = 7, _mv_range = (20,24), _mv_text = "its other abundant natural resource", _mv_resolved_entities = []}], _mg_edges = [MGEdge {_me_relation = "Protagonist", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 6},MGEdge {_me_relation = "Benefit", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 2},MGEdge {_me_relation = "Cause", _me_ismodifier = False, _me_prep = Nothing, _me_start = 2, _me_end = 6},MGEdge {_me_relation = "Effect", _me_ismodifier = False, _me_prep = Nothing, _me_start = 2, _me_end = 5},MGEdge {_me_relation = "Theme", _me_ismodifier = False, _me_prep = Nothing, _me_start = 3, _me_end = 6},MGEdge {_me_relation = "Direction", _me_ismodifier = False, _me_prep = Just "to", _me_start = 3, _me_end = 7}]}
  )
-}

mg3 =
  ( "Authorities say at least 35 people have died."
  , MeaningGraph {_mg_vertices = [MGPredicate {_mv_id = 1, _mv_range = (0,8), _mv_frame = "Statement", _mv_sense = ("say",Verb,"1.1"), _mv_verb = VerbProperty {_vp_index = 1, _vp_lemma = Lemma {unLemma = "say"}, _vp_tense = Present, _vp_aspect = Simple, _vp_voice = Active, _vp_auxiliary = [], _vp_negation = Nothing, _vp_words = [("say",(1,Lemma {unLemma = "say"}))]}},MGPredicate {_mv_id = 2, _mv_range = (2,7), _mv_frame = "Ceasing_to_be", _mv_sense = ("die",Verb,"1.1"), _mv_verb = VerbProperty {_vp_index = 7, _vp_lemma = Lemma {unLemma = "die"}, _vp_tense = Present, _vp_aspect = Perfect, _vp_voice = Active, _vp_auxiliary = [], _vp_negation = Nothing, _vp_words = [("have",(6,Lemma {unLemma = "have"})),("die",(7,Lemma {unLemma = "die"}))]}},MGEntity {_mv_id = 3, _mv_range = (0,0), _mv_text = "Authorities", _mv_resolved_entities = []},MGEntity {_mv_id = 4, _mv_range = (2,5), _mv_text = "at least 35 people", _mv_resolved_entities = []}], _mg_edges = [MGEdge {_me_relation = "Speaker", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 3},MGEdge {_me_relation = "Message", _me_ismodifier = False, _me_prep = Nothing, _me_start = 1, _me_end = 2},MGEdge {_me_relation = "Entity", _me_ismodifier = False, _me_prep = Nothing, _me_start = 2, _me_end = 4}]}
  )
  
test rolemap mg = do
  putStrLn (dotMeaningGraph (mg^._1) (mg^._2))
  print (mkARB rolemap (mg^._2))
  

main = do
  rolemap <- loadRoleInsts "/home/wavewave/repo/srcp/lexicon-builder/mapping/final.txt"
  
  -- test rolemap mg1
  -- test rolemap mg2
  test rolemap mg3
