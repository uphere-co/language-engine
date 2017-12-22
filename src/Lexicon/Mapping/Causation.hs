{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lexicon.Mapping.Causation where

import           Control.Lens
import           Data.Text      (Text)
--
import           Lexicon.Type   (FNFrame,FNFrameElement)

data CauseDual = CauseDual { _cm_baseFrame :: FNFrame
                           , _cm_causativeFrame :: FNFrame
                           , _cm_externalAgent :: FNFrameElement
                           , _cm_extraMapping :: [(FNFrameElement,FNFrameElement)]
                           }
               deriving Show

makeLenses ''CauseDual

causeDualMap =
  [ CauseDual "Process_start"    "Activity_start"       "Agent" [("Event"  ,"Activity")]
  , CauseDual "Process_stop"     "Activity_stop"        "Agent" [("Process","Activity")]
  , CauseDual "Process_end"      "Activity_finish"      "Agent" [("Process","Activity")]
  , CauseDual "Process_continue" "Activity_ongoing"     "Agent" [("Event"  ,"Activity")]
  , CauseDual "Process_resume"   "Activity_resume"      "Agent" [("Process","Activity")]
  , CauseDual "Process_pause"    "Activity_pause"       "Agent" [("Process","Activity")]
  , CauseDual "Undergo_change"   "Cause_change"         "Agent" []
  , CauseDual "Change_position_on_a_scale" "Cause_change_of_position_on_a_scale" "Agent" []
  , CauseDual "Self_motion"      "Cause_motion"         "Agent" [("Self_mover","Theme")]
  , CauseDual "Motion"           "Cause_motion"         "Agent" []
  , CauseDual "Moving_in_place"  "Cause_to_move_in_place" "Agent" []
  , CauseDual "Mass_motion"      "Cause_motion"         "Agent" [("Mass_theme","Theme")]
  , CauseDual "Inclusion"        "Cause_to_be_included" "Agent" [("Part","New_member"),("Total","Group")]
  , CauseDual "Cause_change_of_strength" "Cause_change_of_strength" "Agent" []
  , CauseDual "Relating_concepts" "Make_cognitive_connection" "Cognizer" []
  , CauseDual "Fluidic_motion"   "Cause_fluidic_motion" "Agent" []
  , CauseDual "Expansion"        "Cause_expansion"      "Agent" []
  , CauseDual "Limiting"         "Limiting"             "Agent" []
  , CauseDual "Amalgamation"     "Cause_to_amalgamate"  "Agent" []
  , CauseDual "Make_noise"       "Cause_to_make_noise"  "Agent" [("Sound_source","Sound_maker")]
  , CauseDual "Being_attached"   "Attaching"            "Agent" []
  , CauseDual "Emotion_directed" "Cause_emotion"        "Agent" []
  -- , CauseDual "Separating"       "Separating"           "Agent" []
  , CauseDual "Becoming_separated" "Separating"         "Agent" []
  , CauseDual "Being_located"    "Placing"              "Agent" []
  , CauseDual "Locative_relation" "Placing"             "Agent" [("Figure","Theme"),("Ground","Place")]
  , CauseDual "Abundance"        "Cause_proliferation_in_number" "Agent" [("Collection","Attribute")]
  , CauseDual "Change_of_temperature" "Cause_temperature_change" "Agent" [("Initial_temparature","Temparature_start"),("Final_temperature","Temperature_goal")]
  , CauseDual "Go_into_shape"    "Cause_change"         "Agent" [("Theme","Entity"),("Resultant_configuration","Final_category")]
  , CauseDual "Experience_bodily_harm" "Cause_harm"     "Agent" [("Experiencer","Victim")]
  , CauseDual "Breaking_apart"   "Cause_to_fragment"    "Agent" [("Whole","Whole_patient")]
  , CauseDual "Be_translation_equivalent" "Translating" "Cognizer" []
  , CauseDual "Change_of_consistency" "Cause_change_of_consistency" "Agent" []
  , CauseDual "Occupy_rank"      "Assessing"            "Assessor" [("Item","Phenomenon"),("Rank","Value")]
  , CauseDual "Go_into_shape"    "Manipulate_into_shape" "Agent" []
  , CauseDual "Assemble"         "Gathering_up"         "Agent" [("Group","Aggregate")]
  , CauseDual "Proliferating_in_number" "Cause_proliferation_in_number" "Agent" []
  , CauseDual "Progression"      "Cause_to_make_progress" "Agent" []
  , CauseDual "Traversing"       "Cause_motion"         "Agent" []
  , CauseDual "Impact"           "Cause_impact"         "Agent" []
  , CauseDual "Experiencer_focus" "Cause_to_experience" "Agent" []   -- it's not clear
  , CauseDual "Vehicle_landing"  "Cause_to_land"        "Operator" []
  , CauseDual "Distributed_position" "Dispersal"        "Agent" [("Theme","Individuals"),("Location","Goal_area")]
  , CauseDual "Successful_action" "Success_or_failure" "Agent" []
  , CauseDual "Take_place_of"    "Replacing"            "Agent" []
  , CauseDual "Being_dry"        "Cause_to_be_dry"      "Agent" [("Item","Dryee")]
  , CauseDual "Expend_resource"  "Expend_resource"      "Agent" []
  , CauseDual "Transition_to_state" "Cause_change"      "Agent" [("Initial_state","Initial_category")]
  
  ]
