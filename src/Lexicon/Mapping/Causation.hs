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
  , CauseDual "Separating"       "Separating"           "Agent" []
  , CauseDual "Being_located"    "Placing"              "Agent" []
  , CauseDual "Locative_relation" "Placing"             "Agent" [("Figure","Theme"),("Ground","Place")]
  , CauseDual "Abundance"        "Cause_proliferation_in_number" "Agent" [("Collection","Attribute")]
  ]
