{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lexicon.Mapping.Causation where

import           Control.Lens
import           Data.Text      (Text)


data CauseDual = CauseDual { _cm_baseFrame :: Text
                           , _cm_causativeFrame :: Text
                           , _cm_externalAgent :: Text
                           , _cm_extraMapping :: [(Text,Text)]
                           }
               deriving Show

makeLenses ''CauseDual

causeDualMap =
  [ CauseDual "Process_start"    "Activity_start"   "Agent" [("Event"  ,"Activity")]
  , CauseDual "Process_stop"     "Activity_stop"    "Agent" [("Process","Activity")]
  , CauseDual "Process_end"      "Activity_finish"  "Agent" [("Process","Activity")]
  , CauseDual "Process_continue" "Activity_ongoing" "Agent" [("Event"  ,"Activity")]
  , CauseDual "Process_resume"   "Activity_resume"  "Agent" [("Process","Activity")]
  , CauseDual "Process_pause"    "Activity_pause"   "Agent" [("Process","Activity")]
  , CauseDual "Undergo_change"   "Cause_change"     "Agent" []
  , CauseDual "Change_position_on_a_scale" "Cause_change_of_position_on_a_scale" "Agent" []
  ] 
