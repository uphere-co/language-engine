{-# LANGUAGE OverloadedStrings #-}

module OntoNotes.Mapping.FrameNet where

import Data.Text  (Text)


mapFromONtoFN :: [ (Text, [(Text,Text)]) ]
mapFromONtoFN =
    [ ( "be"
      , [("1.1", "copula")
        ,("1.2", "copula")
        ,("1.3", "Existence")
        ,("1.4", "Being_located")
        ,("1.5", "Event")
        ,("1.6", "Using_resource")
        ,("1.7", "Expensiveness")
        ,("1.9", "none")])

    , ( "say"
      , [("1.1", "Statement")
        ,("1.2", "Sign")
        ,("1.3", "Conditional_occurrence")
        ,("1.4", "Statement")
        ,("1.5", "Spelling_and_pronouncing")
        ,("1.6", "none")])

    , ( "have"
      , [("1.1", "Possession")
        ,("1.2", "copula") -- "(copula)")
        ,("1.3", "Being_employed")
        ,("1.4", "Undergoing")  --  || Perception_experience")
        ,("1.5", "Personal_relationship")
        ,("1.6", "Causation")
        ,("1.7", "Getting") -- "(Getting)")
        ,("1.8", "Arranging")
        ,("1.9", "Giving_birth")
        ,("1.10", "Sex")
        ,("1.11", "idioms")
        ,("1.12", "none")])

    , ( "make"
      , [("1.1", "lightverb")
        ,("1.2", "Creating") -- "Creating < Manufacturing")
        ,("1.3", "Causation")
        ,("1.4", "Causation") --  <- force.v")
        ,("1.6", "Coming_to_be") -- "(Coming_to_be) || (Becoming)")
        ,("1.7", "Getting")
        ,("1.8", "Accomplishment")
        ,("1.9", "Self_motion")
        ,("1.15", "Conduct")
        ,("1.16", "idioms")])

    , ( "take"
      , [("1.1", "Intentionally_act")
        ,("1.2", "lightverb")
        ,("1.3", "Taking_time") --  || Have_as_requirement")
        ,("1.4", "Bringing")  --  "/ Taking")
        ,("1.5", "Taking")
        ,("1.6", "Coming_to_be") -- "(Coming_to_be)")
        ,("1.7", "Categorization")  -- "Categorization || Communicate_categorization || Regard")
        ,("1.8", "Ride_vehicle")
        ,("1.9", "Being_in_control")
        ,("1.11", "Removing") --  "|| Taking")
        ,("1.12", "Soaking_up") --  "(Soaking_up)")
        ,("1.14.1", "idioms")
        ,("1.15", "none")])

    , ( "get"
      , [("1.1", "Getting") -- "Getting || Grasp")
        ,("1.2", "Medical_conditions")
        ,("1.3", "Becoming")
        ,("1.4", "Arriving")
        ,("1.11.1", "Becoming")
        ,("1.11.2", "Arriving")
        ,("1.11.3", "Activity_start")
        ,("1.11.5", "Activity_ongoing")  -- "(Activity_continue) | (State_continue)")
        ,("1.11.6", "Arriving")
        ,("1.11.7", "Activity_finish")
        ,("1.11.8", "Becoming_aware")
        ,("1.11.13", "Personal_relationship") -- "(Personal_relationship)")
        ,("1.11.17", "Recovery")
        ,("1.11.19", "Avoiding")
        ,("1.11.20", "Assistance") -- "(Assistence)")
        ,("1.11.23", "Getting")
        ,("1.11.26", "Experiencer_focus")
        ,("1.11.33", "Waking_up")
        ,("1.11.37", "idioms")
        ,("1.11.38", "none")
        ,("1.12", "none")])

    , ( "sell"
      , [("1.1", "Commerce_sell")
        ,("1.2", "Suasion")
        ,("1.3", "Surrendering_possession") -- "(Surrendering_possession)")
        ,("1.5", "Commerce_sell")
        ,("1.6", "Commerce_sell")])

    , ( "go"
      , [("1.1", "Motion")
        ,("1.2", "Motion")
        ,("1.3", "Being_operational")
        ,("1.4", "Becoming")
        ,("1.5", "Event")
        ,("1.7", "Purpose")
        ,("1.8", "Inclusion")
        ,("1.9", "Attending")
        ,("1.10", "Path_shape")  -- "(Path_shape)")
        ,("1.11", "Being_operational")
        ,("1.13", "Taking_sides")
        ,("1.14", "lightverb")
        ,("1.15", "idioms")])

    , ( "expect"
      , [("1.1", "Expectation")
        ,("1.2", "Have_as_requirement")])

    , ( "use"
      , [("1.1", "Using")
        ,("1.2", "Ingest_substance")
        ,("1.3", "Expend_resource")
        ,("1.4", "Frequency")]) -- "(Frequency)")])

    , ( "do"
      , [("1.1", "Intentionally_act")
        ,("1.2", "Thriving")
        ,("1.3", "Meet_specifications")
        ,("1.5", "Successful_action") -- "(Successful_action)")
        ,("1.7", "Intentionally_affect")])

    , ( "come"
      , [("1.1", "Arriving")
        ,("1.2", "Event")
        ,("1.3", "Becoming")
        ,("1.4", "copula") --   "(copula)")
        ,("1.5", "Origin")
        ,("1.6", "Event")   --  "(Event) || (Coming_to_be)")
        ,("1.7", "Amounting_to")   --  "(Amounting_to)")
        ,("1.8", "Coming_up_with")         -- "Coming_up_with || Becoming_aware")
        ,("1.9", "Cause_to_perceive")
        ,("1.11", "idioms")])

    , ( "buy"
      , [("1.1", "Commerce_buy")
        ,("1.2", "Attempt_suasion")
        ,("1.3", "Coming_to_believe")
        ,("1.4", "Funding")
        ,("1.6", "Taking")])

    , ( "include"
      , [("1.1", "Categorization")  --   "Categorization || Inclusion")
        ,("1.2", "Inclusion")])

    , ( "give"
      , [("1.1", "Giving")
        ,("1.2", "Offering")
        ,("1.3", "Deny_or_grant_permission")  -- "(Deny_or_grant_permission) || (Reward_and_punishments)")
        ,("1.4", "Giving")  -- "(Giving) || (Eventive_affecting)")
        ,("1.5", "Success_or_failure")
        ,("1.6", "Sign")
        ,("1.7", "Arranging")
        ,("1.8", "Giving")
        ,("1.9", "Emitting")
        ,("1.10", "Estimating")
        ,("1.14", "idioms")
        ,("1.15", "none")])

    , ( "rise"
      , [("1.1", "Motion_directional")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.6", "Origin")])

    , ( "pay"
      ,  [("1.1", "Commerce_pay")
        ,("1.2", "Attention")  -- (Attention)
        ,("1.3", "Deserving")   -- (Deserving)
        ,("1.4", "Intentionally_act")  -- (Intentionally_act)
        ,("1.5", "none")])

    , ( "see"
      , [("1.1", "Perception_experience")
        ,("1.2", "Grasp")
        ,("1.3", "Perception_experience")
        ,("1.4", "Verification")
        ,("1.5", "Attention")   -- (Attention)
        ,("1.6", "Meet_with")])

    , ( "want"
      , [("1.1", "Desiring")
        ,("1.2", "Desiring")
        ,("1.3", "none")])

    , ( "add"
      , [("1.1", "Cause_to_be_included")
        ,("1.2", "Cause_change_of_position_on_a_scale")])

    , ( "call"
      , [("1.1", "Referring_by_name")
        ,("1.2", "Contacting")
        ,("1.3", "Request")
        ,("1.4", "Contacting")
        ,("1.5", "Labeling")
        ,("1.6", "Request")
        ,("1.8", "Statement")   -- (Statement)
        ,("1.9", "Request")
        ,("1.11", "Cause_to_end")])   -- Cause_to_end <- abort.v

    , ( "continue"
      , [("1.1", "Process_continue")  -- Process_continue, Activity_ongoing
        ,("1.2", "Statement")])  -- Statement, Activity_ongoing

    , ( "think"
      , [("1.1", "Opinion")
        ,("1.2", "Cogitation")
        ,("1.3", "Remembering_experience")  --  Remembering_experience || Remembering_information
        ,("1.4", "Experiencer_focus")
        ,("1.7", "none")])

    , ( "hold"
      , [("1.1", "Manipulation")
        ,("1.2", "Detaining") -- "(Detaining) || (Inhibit_movement) || (Thwarting)")
        ,("1.3", "Activity_ongoing")  -- "(Activity_ongoing)")
        ,("1.4", "Statement") -- "(Statement <- maintain.v)")
        ,("2.5", "Possession") -- "Possession || Being_in_control")
        ,("1.6", "Containing")
        ,("1.7", "Being_relevant")
        ,("1.8", "Arranging")
        ,("1.10", "idioms")])

    , ( "begin"
      , [("1.1", "Process_start")
        ,("1.2", "Activity_start")
        ,("1.3", "Activity_start")
        ,("1.4", "Sufficiency")])

    , ( "fall"
      , [("1.1", "Change_position_on_a_scale") -- "Change_poisition_on_a_scale || Motion_directional")
        ,("1.2", "Coming_to_be")
        ,("1.3", "Catastrophe") -- "(Catastrophe)")
        ,("1.4", "Event") -- "(Event)")
        ,("1.12", "Being_operational")
        ,("1.14", "Make_agreement_on_action")])

    , ( "know"
      , [("1.1", "Awareness")
        ,("1.2", "Verification") -- "(Verification)")
        ,("1.3", "Awareness") -- "(Awareness)")
        ,("1.4", "Perception_experience") -- "(Perception_experience)")
        ,("1.5", "Differentiation") -- "(Differentiation) || (Affirm_or_deny)")
        ,("1.7", "none")])

    , ( "find"
      , [("1.1", "Locating") -- "Locating || Becoming_aware")
        ,("1.2", "Resolve_problem")
        ,("1.3", "Coming_to_believe") -- "(Coming_to_believe)")
        ,("1.4", "Assessing") -- "(Assessing)")
        ,("1.5", "Verdict")])

    , ( "offer"
      , [("1.1", "Offering")
        ,("1.2", "Cause_to_perceive")
        ,("1.3", "Commitment")
        ,("1.4", "Communication") -- "(Communication)")
        ,("1.5", "Publishing")])

    , ( "become"
      , [("1.1", "Becoming")])

    , ( "help"
      , [("1.1", "Assistance")
        ,("1.3", "Avoiding")])

    , ( "increase"
      , [("1.1", "Change_position_on_a_scale")])

    , ( "show"
      , [("1.1", "Cause_to_perceive") -- "Cause_to_perceive || Reveal_secret || Evidence")
        ,("1.2", "Arriving")
        ,("1.3", "Publishing")
        ,("1.6", "Request")
        ,("1.7", "Intentionally_act")])

    , ( "provide"
      , [("1.1", "Supply")
        ,("1.2", "Supply")
        ,("1.3", "Have_as_requirement")]) -- "(Have_as_requirement)")])

    , ( "try"
      , [("1.1", "Attempt")
        ,("1.2", "Experimentation")
        ,("1.3", "Try_defendant")])

    , ( "end"
      , [("1.1", "Process_end")
        ,("1.2", "Cause_to_end")
        ,("1.3", "Process_end")
        ,("1.4", "Transition_to_a_situation")])

    , ( "work"
      , [("1.1", "Being_employed")
        ,("1.2", "Being_operational")
        ,("1.3", "Manipulate_into_shape")
        ,("1.6", "Using")
        ,("1.8", "Resolve_problem")
        ,("1.9", "idioms")])

    , ( "report"
      , [("1.1", "Statement")
        ,("1.3", "Communication")]) -- "(Communication)")])

    , ( "remain"
      , [("1.1", "State_continue")
        ,("1.2", "Remainder")])

    , ( "put"
      , [("1.1", "Placing")
        ,("1.2", "lightverb")
        ,("1.3", "Statement")
        ,("1.4", "Using_resource")
        ,("1.5", "Estimating")
        ,("1.6", "Causation") -- "(Causation)")
        ,("1.7.1", "Statement") -- "(Statement)")
        ,("1.7.2", "Placing")
        ,("1.7.3", "Commitment")
        ,("1.7.4", "Using_resource")
        ,("1.7.11", "Judgment_communication")
        ,("1.7.12", "Recording")
        ,("1.7.15", "Change_event_time")
        ,("1.7.17", "Cause_to_end")
        ,("1.7.19", "Installing")
        ,("1.7.21", "Manufacturing") -- "(Manufacturing) || (Product_development)")
        ,("1.7.24", "Storing")
        ,("1.7.28", "Cause_to_be_included")
        ,("1.7.29", "Tolerating")
        ,("1.7.32", "none")])

    , ( "raise"
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2", "Cause_motion")
        ,("1.3", "Communication_manner")
        ,("1.4", "Cause_change_of_position_on_a_scale")
        ,("1.5", "Growing_food") -- "Growing_food || (Personal_relationship)")
        ,("1.6", "Cause_to_perceive") -- "Cause_to_perceive || Topic")
        ,("1.9", "Cause_to_end")])

    , ( "lead"
      , [("1.1", "Causation")
        ,("1.2", "Cotheme")
        ,("1.3", "Path_shape") -- "(Path_shape)")
        ,("1.4", "Leadership")
        ,("1.5", "Cotheme")])

    , ( "look"
      , [("1.1", "Scrutiny") -- "Scrutiny || Perception_active")
        ,("1.2", "Give_impression")
        ,("1.4", "Attention") -- "(Attention)")
        ,("1.5", "Waiting")
        ,("1.6", "Reliance")])

    , ( "need"
      , [("1.1", "Needing")
        ,("1.2", "Have_as_requirement")])

    , ( "agree"
      , [("1.1", "Make_agreement_on_action")
        ,("1.3", "Agree_or_refuse_to_act")])

    , ( "tell"
      , [("1.1", "Telling")
        ,("1.2", "Differentiation")])

    , ( "run"
      , [("1.1", "Self_motion")
        ,("1.2", "Path_shape")
        ,("1.3", "Operating_a_system")
        ,("1.4", "Motion")
        ,("1.6", "Cause_to_perceive") -- "(Cause_to_perceive)")
        ,("1.7", "Seeking_to_achieve") -- "(Seeking_to_acheive)")
        ,("1.10", "Commerce_scenario") -- "(Commerce_scenario) || (Committing_crime) || (Legality)")
        ,("1.13.4", "Expend_resource")
        ,("1.13.5", "Meet_with") -- "Meet_with || Confronting_problem")
        ,("1.13.17", "Change_position_on_a_scale")
        ,("1.13.21", "none")
        ,("1.14", "none")])

    , ( "keep"
      , [("1.1", "Retaining")
        ,("1.2", "Retaining")
        ,("1.3", "Retaining")
        ,("1.4", "Preventing_or_letting")
        ,("1.5", "Compliance") -- "(Compliance)")
        ,("1.7", "Perception_active") -- "(Perception_active)")
        ,("1.8.12", "none")])

    , ( "believe"
      , [("1.1", "Certainty")
        ,("1.2", "Religious_belief")])

    , ( "follow"
      , [("1.1", "Cotheme")
        ,("1.2", "Relative_time")
        ,("1.3", "Compliance")
        ,("1.4", "Perception_active")
        ,("1.5", "Evidence") -- "(Evidence) || (Causation)")
        ,("1.6", "Process_continue")])

    , ( "seem"
      , [("1.1", "Give_impression")])

    , ( "consider"
      , [("1.1", "Categorization")
        ,("1.2", "Cogitation")])

    , ( "seek"
      , [("1.1", "Seeking")])

    , ( "receive"
      , [("1.1", "Receiving")
        ,("1.2", "Adopt_selection")]) -- "(Adopt_selection)")])

    , ( "build"
      , [("1.1", "Building")
        ,("1.2", "Coming_to_be") -- "(Coming_to_be)")
        ,("1.3", "Creating") -- "(Creating)")
        ,("1.5", "Cause_to_be_included")])

    , ( "plan"
      , [("1.1", "Purpose")
        ,("1.2", "Making_arrangements")])

    , ( "grow"
      , [("1.1", "Becoming")
        ,("1.2", "Transition_to_state")
        ,("1.3", "Ontogeny")
        ,("1.4", "Expansion")
        ,("1.5", "Transition_to_state")])

    , ( "decline"
      , [("1.1", "Improvement_or_decline")
        ,("1.2", "Agree_or_refuse_to_act")
        ,("1.3", "Change_position_on_a_scale")])

    , ( "ask"
      , [("1.1", "Questioning")
        ,("1.2", "Request")
        ,("1.3", "Request")
        ,("1.4", "Request")])

    , ( "produce"
      , [("1.1", "Manufacturing")
        ,("1.2", "Creating")])

    , ( "base"
      , [("1.1", "Reasoning") -- "(Reasoning? Evidence?)")
        ,("1.3", "Placing")])

    , ( "leave"
      , [("1.1", "Departing")
        ,("1.5", "Inclusion")])

    , ( "set"
      , [("1.1", "Placing")
        ,("1.2", "Motion_directional")
        ,("1.3", "Arranging")
        ,("1.4", "Arranging")
        ,("1.5", "Causation")
        ,("1.6", "Cause_to_start")
        ,("1.8", "Arranging")])

    , ( "move"
      , [("1.1", "Motion") -- "Motion - Cause_motion")
        ,("1.2", "Intentionally_act")
        ,("1.3", "Experiencer_obj")
        ,("1.4", "Residence")
        ,("1.5", "Commerce_sell") -- "(Commerce_sell)")
        ,("1.6", "Social_connection") -- "(Social_connection)")
        ,("1.7", "Experiencer_obj")]) -- "(Experiencer_obj) || (Attack)")])

    , ( "turn"
      , [("1.1", "Change_direction")
        ,("1.2", "Becoming")
        ,("1.3", "Undergo_change")
        ,("1.4", "Agree_or_refuse_to_act")
        ,("1.5", "Cause_change")
        ,("1.6", "Turning_out")
        ,("1.7", "Becoming_visible")
        ,("1.8", "Manufacturing")
        ,("1.11", "Submitting_documents") -- "Submitting_documents || (Giving (?))")
        ,("1.13", "Improvement_or_decline")
        ,("1.14", "Gathering_up")
        ,("1.15", "idioms")])

    , ( "start"
      , [("1.1", "Process_start")
        ,("1.2", "Activity_start")
        ,("1.3", "Cause_to_start")
        ,("1.4", "Activity_start")]) -- "Activity_start - Cause_to_start")])

    , ( "allow"
      , [("1.1", "Preventing_or_letting")
        ,("1.2", "Capability")])

    , ( "require"
      , [("1.1", "Needing")
        ,("1.2", "Imposing_obligation")])

    , ( "create"
      , [("1.1", "Creating")])

    , ( "lose"
      , [("1.1", "Losing_track_of_theme")
        ,("1.2", "Success_or_failure")])

    , ( "reduce"
      , [("1.1", "Cause_change_of_position_on_a_scale")])

    , ( "cut"
      , [("1.1", "Cutting")
        ,("1.2", "Cause_change_of_position_on_a_scale")
        ,("1.3", "Intentionally_create")
        ,("1.5", "Halt")])

    , ( "close"
      , [("1.1", "Business_closure")
        ,("1.2", "Closure")
        ,("1.3", "Activity_finish")])

    , ( "bring"
      , [("1.1", "Bringing")
        ,("1.2", "Causation")
        ,("1.3", "Cause_to_perceive")])

    , ( "own"
      , [("1.1", "Possession")])

    , ( "file"
      , [("1.1", "Submitting_documents")
        ,("1.3", "Self_motion")
        ,("1.4", "Placing")])

    , ( "name"
      , [("1.1", "Name_conferral")
        ,("1.2", "Indicating")
        ,("1.3", "Appointing")])

    , ( "announce"
      , [("1.1", "Statement")])

    , ( "appear"
      , [("1.1", "Give_impression")
        ,("1.2", "Coming_to_be")
        ,("1.3", "Performers_and_roles")])

    , ( "estimate"
      , [("1.1", "Estimating")])

    , ( "develop"
      , [("1.1", "Intentionally_create")
        ,("1.2", "Ontogeny")])

    , ( "reach"
      , [("1.1", "Arriving")
        ,("1.2", "Contacting")
        ,("1.5", "Causation")])

    , ( "open"
      , [("1.1", "Change_accessibility")
        ,("1.2", "Activity_start")
        ,("1.5", "Use_firearm")])

    , ( "note"
      , [("1.1", "Becoming_aware")
        ,("1.2", "Statement")])

    , ( "change"
      , [("1.1", "Undergo_change")
        ,("1.2", "Replacing")
        ,("1.3", "Exchange")])

    , ( "spend"
      , [("1.1", "Expend_resource")     -- "                  (Expend_resource? time.)")
        ,("1.2", "Expend_resource")
        ,("1.3", "Using_resource")])

    , ( "acquire"
      , [("1.2", "Cause_to_perceive")
        ,("1.3", "Getting")])

    , ( "involve"
      , [("1.1", "Experiencer_obj")
        ,("1.2", "Inclusion")])

    , ( "cause"
      , [("1.1", "Causation")])

    , ( "meet"
      , [("1.1", "Make_acquaintance")
        ,("1.2", "Meet_with")
        ,("1.3", "Locative_relation")
        ,("1.4", "Meet_specifications")
        ,("1.5", "Undergoing")])

    , ( "win"
      , [("1.1", "Beat_opponent") -- "Win_prize || Beat_opponent")
        ,("1.2", "Getting")        -- "Win_prize || Getting")
        ,("1.3", "Cause_change")])

    , ( "drop"
      , [("1.1", "Motion_directional")  -- "Motion_directional || Cause_motion")
        ,("1.2", "Change_position_on_a_scale") -- "Change_position_on_a_scale || Cause_to_end")
        ,("1.3", "Communication")
        ,("1.6", "Expend_resource")
        ,("1.8", "Forgoing")
        ,("1.13.4", "Surrendering")]) -- "Surrendering || Abandonment")])

    , ( "operate"
      , [("1.1", "Being_in_operation")
        ,("1.2", "Operating_a_system")])

    , ( "feel"
      , [("1.1", "Feeling") -- ("Feeling || Perception_active || Perception_experience")
        ,("1.3", "Opinion")])

    , ( "trade"
      , [("1.1", "Commercial_transaction")
        ,("1.2", "Exchange")
        ,("1.4", "Reliance")])

    , ( "mean"
      , [("1.1", "Topic")
        ,("1.2", "Purpose")
        ,("1.3", "Linguistic_meaning")
        ,("1.4", "Evidence")
        ,("1.5", "Importance")])

    , ( "propose"
      , [("1.1", "Attempt_suasion")
        ,("1.2", "Purpose")])

    , ( "write"
      , [("1.1", "Text_creation")
        ,("1.2", "Spelling_and_pronouncing")
        ,("1.5", "Cause_change_of_position_on_a_scale")])

    , ( "yield"
      , [("1.1", "Earnings_and_losses") -- "Earnings_and_losses || Creating")
        ,("1.2", "Surrendering_possession")])

    , ( "suggest"
      , [("1.1", "Attempt_suasion")
        ,("1.2", "Evidence")])

    , ( "represent"
      , [("1.1", "Representing")
        ,("1.2", "Representative")
        ,("1.3", "Cause_to_perceive")])

    , ( "face"
      , [("1.1", "Confronting_problem")
        ,("1.2", "Directional_locative_relation")])

    , ( "decide"
      , [("1.1", "Deciding")
        ,("1.2", "Eventive_affecting")])

    , ( "hope"
      , [("1.1", "Desiring")])

    , ( "approve"
      , [("1.1", "Deny_or_grant_permission")])

    , ( "send"
      , [("1.1", "Sending")
        ,("1.2", "Causation")
        ,("1.3", "Causation")])

    , ( "play"
      , [("1.1", "Activity") -- "                          (Activity?)")
        ,("1.2", "Competition")
        ,("1.3", "Function") -- "                          (Function, Being_relevant, Importance?)")
        ,("1.4", "Performers_and_roles")
        ,("1.5", "Performers_and_roles")
        ,("1.6", "Wagering")
        ,("1.7", "Prevarication") -- "(Prevarication? <- kid.v)")
        ,("1.12.6", "Attention") -- "                       (Attention? <- ignore.v)")
        ,("1.12.7", "Competition")
        ,("1.12.13", "none")
        ,("1.13", "none")])

    , ( "issue"
      , [("1.1", "Emanating")])

    , ( "disclose"
      , [("1.1", "Reveal_secret")])

    , ( "fail"
      , [("1.1", "Success_or_failure")
        ,("1.2", "Being_operational")
        ,("1.3", "Attention")])

    , ( "manage"
      , [("1.1", "Successful_action")
        ,("1.2", "Operating_a_system")])

    , ( "force"
      , [("1.1", "Causation")
        ,("1.2", "Cause_motion")
        ,("1.4", "Firing")])

    , ( "carry"
      , [("1.1", "Bringing")
        ,("1.2", "Containing") -- "(Containing <- contain.v)")
        ,("1.3", "Being_obligated") -- "(Being_obligated)")
        ,("1.5", "Conduct")
        ,("1.6", "Emotion_directed") -- "(Emotion_directed <- mad.a)")
        ,("1.8", "Activity_ongoing")]) -- "(Activity_ongoing)")])

    , ( "pass"
      , [("1.1", "Passing")
        ,("1.2", "Success_or_failure")
        ,("1.3", "Process_end")
        ,("1.4", "Giving")
        ,("1.7", "Path_shape")
        ,("1.8", "Giving")
        ,("1.11", "Verification") -- "(Verification)")
        ,("1.14", "none")])

    , ( "improve"
      , [("1.1", "Cause_to_make_progress")])

    , ( "happen"
      , [("1.1", "Event")
        ,("1.2", "Coincidence")])

    , ( "argue"
      , [("1.1", "Quarreling")
        ,("1.2", "Reasoning")
        ,("1.3", "Evidence")])

    , ( "support"
      , [("1.1", "Supporting")
        ,("1.2", "Taking_sides")
        ,("1.3", "Evidence")])

    , ( "succeed"
      , [("1.1", "Success_or_failure") -- "Success_or_failure   || Successful_action || Personal_success")
        ,("1.2", "Take_place_of")])

    , ( "charge"
      , [("1.1", "Commerce_collect")
        ,("1.2", "Imposing_obligation")
        ,("1.3", "Judgment_communication")])

    , ( "reflect"
      , [("1.1", "Evidence")]) -- "(Evidence <- show.v)")])

    , ( "return"
      , [("1.1", "Arriving") -- "Arriving || Rejuvenation || Resurrection")
        ,("1.2", "Repayment") -- "(Repayment <- pay back.v, Earnings_and_losses <- earn.v)")
        ,("1.3", "Response")
        ,("1.4", "Response")])

    , ( "cover"
      , [("1.1", "Distributed_position") -- "Distributed_position      (how to make causative?)")
        ,("1.2", "Topic") -- "Topic || ( Sufficiency <- suffice.v)")
        ,("1.3", "Protecting")
        ,("1.4", "Hiding_objects") -- "Hiding_objects || Prevarication")
        ,("1.8", "none")])

    , ( "expand"
      , [("1.1", "Expansion")]) -- "Expansion - Cause_expansion")])

    , ( "serve"
      , [("1.1", "Serving_in_capacity")
        ,("1.2", "Function")
        ,("1.3", "Being_obligated") -- "                    (Being_obligated)")
        ,("1.4", "Giving") -- "                    (Giving)")
        ,("1.5", "Imprisonment")])

    , ( "complete"
      , [("1.1", "Activity_finish")])

    , ( "introduce"
      , [("1.1", "Make_acquaintance")
        ,("1.2", "Process_start")]) -- "(Process_start, Achieving_first, First_experience)")])

    , ( "stop"
      , [("1.1", "Activity_stop")
        ,("1.2", "Thwarting") -- "Thwarting || Preventing_or_letting")
        ,("1.3", "Visiting")])

    , ( "live"
      , [("1.1", "Manner_of_life")
        ,("1.2", "Residence")
        ,("1.3", "Activity_ongoing")
        ,("1.4", "Subsisting")
        ,("1.5.1", "Subsisting")
        ,("1.5.6", "Emotions_of_mental_activity")
        ,("1.5.8", "none")])

    , ( "let"
      , [("1.1", "Preventing_or_letting")
        ,("1.3", "none")])

    , ( "price"
      , [("1.1", "Commerce_scenario")])

    , ( "head"
      , [("1.1", "Self_motion")
        ,("1.2", "Leadership") -- "Leadership || Relative_time")
        ,("1.6", "Thwarting")])

    , ( "push"
      , [("1.1", "Cause_motion")
        ,("1.2", "Seeking_to_achieve")
        ,("1.3", "Attempt_suasion") -- "(Attempt_suasion)")
        ,("1.6.1", "Cause_motion")
        ,("1.6.2", "Ontogeny")]) -- "(Ontogeny)")])

    , ( "purchase"
      , [("1.1", "Commerce_buy")])

    , ( "hit"
      , [("1.1", "Hit_target")
        ,("1.2", "Event") -- -- "Event || Impact")
        ,("1.3", "Arriving")]) -- "Arriving || Hit_target")])

    , ( "join"
      , [("1.1", "Cause_to_amalgamate") -- "Cause_to_amalgamate || Attaching")
        ,("1.2", "Becoming_a_member")
        ,("1.3", "Participation")])

    , ( "indicate"
      , [("1.1", "Evidence")
        ,("1.2", "Indicating")
        ,("1.3", "Communication")])

    , ( "accept"
      , [("1.1", "Respond_to_proposal")
        ,("1.2", "Tolerating")
        ,("1.3", "Coming_to_believe")
        ,("1.4", "Institutionalization") -- "(Institutionalization <- admit.v)")
        ,("1.5", "Retaining")]) -- "(Retaining <- retain.v)")])

    , ( "talk"
      , [("1.1", "Communication")
        ,("1.2", "Reveal_secret")
        ,("1.3", "Speak_on_topic")
        ,("1.9", "none")])

    , ( "predict"
      , [("1.1", "Predicting")])

    , ( "back"
      , [("1.2", "Taking_sides") -- "Taking_sides || Funding || Evidence")
        ,("1.4", "Duplication")
        ,("1.5", "Cause_to_end") -- "(Cause_to_end <- abort.v, Withdraw_from_participation <- withdraw.v)")
        ,("1.6", "Cause_to_end") -- "(Cause_to_end <- abort.v, Withdraw_from_participation <- withdraw.v)")
        ,("1.9", "none")])

    , ( "cost"
      , [("1.1", "Expensiveness")])

    , ( "invest"
      , [("1.1", "Funding") -- "Funding || (Commmerce_buy)")
        ,("1.2", "Have_associated")]) -- "(Have_associated <- have.v)")])

    , ( "break"
      , [("1.1", "Experience_bodily_harm") -- "Experience_bodily_harm - Cause_harm")
        ,("1.2", "Render_nonfunctional")
        ,("1.3", "Process_stop") -- "(Process_stop <- stop.v)")
        ,("1.4", "Compliance")
        ,("1.5", "Compliance") -- "(Compliance <- obey.v)")
        ,("1.7", "Event")
        ,("1.16.1", "Breaking_apart")
        ,("1.16.2", "Render_nonfunctional")
        ,("1.16.3", "Process_stop") --  "(Process_stop <- stop.v)")
        ,("1.16.4", "Process_start")
        ,("1.16.5", "Placing") -- "(Placing <- insert.v)")
        ,("1.16.9", "Event") -- "(Event?, Statement?, Process_completed_state?)")
        ,("1.16.11", "none")])

    , ( "post"
      , [("1.1", "Publishing")])

    , ( "claim"
      , [("1.1", "Statement")
        ,("1.2", "Claim_ownership")])

    , ( "maintain"
      , [("1.1", "Activity_ongoing")
        ,("1.2", "Statement")])

    , ( "settle"
      , [("1.1", "Being_located")
        ,("1.2", "Resolve_problem")
        ,("1.3", "Colonization")
        ,("1.4", "Cause_change_of_consistency") -- "(Cause_change_of_consistency <- set.v)")
        ,("1.5", "Motion_directional")]) -- "(Motion_directional <- drop.v)")])

    , ( "quote"
      , [("1.1", "Statement") -- "(Statement)")
        ,("1.2", "Reference_text")
        ,("1.3", "Commerce_scenario")])

    , ( "stand"
      , [("1.1", "Posture") -- "Posture - Change_posture")
        ,("1.2", "Being_located") -- "Being_located - Placing")
        ,("1.3", "Give_impression") -- "(Give_impression <- seem.v, copula)")
        ,("1.4", "Tolerating")
        ,("1.6", "Being_obligated") -- "(Being_obligated)")
        ,("1.7.2", "Taking_sides")
        ,("1.7.4", "Representing")
        ,("1.7.9", "Compliance") -- "(Compliance <- disobey.v, Taking_sides)")
        ,("1.7.11", "Cause_to_perceive") -- "(Cause_to_perceive)")
        ,("1.7.13", "Tolerating")
        ,("1.8", "none")])

    , ( "comment"
      , [("1.1", "Statement")
        ,("1.2", "Statement")])

    , ( "sign"
      , [("1.1", "Sign_agreement")
        ,("1.3", "Create_physical_artwork")])

    , ( "prevent"
      , [("1.1", "Thwarting")])

    , ( "design"
      , [("1.1", "Intentionally_create") -- "(Intentionally_create <- create.v)")
        ,("1.2", "Coming_up_with")
        ,("1.3", "Purpose")])

    , ( "result"
      , [("1.1", "Causation")])

    , ( "oppose"
      , [("1.1", "Taking_sides")
        ,("1.2", "Competition")])

    , ( "vote"
      , [("1.1", "Choosing") -- "(Choosing? Political_actions?, Deciding?)")
        ,("1.4", "Choosing")]) -- "(Choosing? Political_actions?, Deciding?)")])

    , ( "cite"
      , [("1.1", "Referring_by_name")
        ,("1.2", "Adducing")
        ,("1.3", "Judgment_communication")
        ,("1.4", "Citing")])

    , ( "affect"
      , [("1.1", "Objective_influence")])

    , ( "gain"
      , [("1.1", "Getting")
        ,("1.2", "Change_position_on_a_scale")])

    , ( "control"
      , [("1.1", "Control")])

    , ( "total"
      , [("1.1", "Amounting_to")])

    , ( "hear"
      , [("1.1", "Becoming_aware")
        ,("1.2", "Perception_experience")
        ,("1.3", "Court_examination")])

    , ( "exist"
      , [("1.1", "Existence")
        ,("1.2", "Subsisting")])

    , ( "establish"
      , [("1.1", "Intentionally_create")
        ,("1.2", "Explaining_the_facts")])

    , ( "stay"
      , [("1.1", "State_continue")
        ,("1.5.2", "Avoiding")
        ,("1.5.9", "none")])

    , ( "schedule"
      , [("1.1", "Scheduling")
        ,("1.2", "Activity_prepare")])

    , ( "resign"
      , [("1.1", "Quitting")])

    , ( "limit"
      , [("1.1", "Limiting")])

    , ( "handle"
      , [("1.2", "Resolve_problem")
        ,("1.3", "Manipulation")])

    , ( "wait"
      , [("1.1", "Waiting")])

    , ( "protect"
      , [("1.1", "Protecting")
        ,("1.2", "Assistance")])

    , ( "insist"
      , [("1.1", "Statement")
        ,("1.2", "Statement")])

    , ( "explain"
      , [("1.1", "Explaining_the_facts")
        ,("1.2", "Explaining_the_facts")]) -- "Explaining_the_facts (Justifying?)")])

    , ( "deny"
      , [("1.1", "Affirm_or_deny")
        ,("1.2", "Prevent_or_allow_possession")])

    , ( "avoid"
      , [("1.1", "Avoiding")])

    , ( "watch"
      , [("1.1", "Perception_active")
        ,("1.2", "Be_on_alert") -- "Be_on_alert <- alert.v             * Attention")
        ,("1.3", "Scrutiny")]) -- "(Scrutiny <- monitor.v)")])

    , ( "replace"
      , [("1.1", "Replacing")]) --  "Replacing - Take_place_of")])

    , ( "assume"
      , [("1.1", "Conditional_occurrence") -- "Awareness || Conditional_occurence")
        ,("1.2", "Activity_start")]) --  "(Activity_start <- begin.v)")])

    , ( "reject"
      , [("1.1", "Respond_to_proposal")])

    , ( "kill"
      , [("1.1", "Killing")
        ,("1.3", "Erasing") -- "Erasing || (Process_stop?)")
        ,("1.4", "Thwarting")])

    , ( "ease"
      , [("1.1", "Hindering") -- "(Hindering?)")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.3", "Motion")]) -- "Motion - Cause_motion")])

    , ( "drive"
      , [("1.1", "Operate_vehicle")
        ,("1.2", "Causation")
        ,("1.5", "Self_motion")])

    , ( "boost"
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2", "Cause_motion")])

    , ( "launch"
      , [("1.1", "Shoot_projectiles")
        ,("1.2", "Activity_start")
        ,("1.3", "Activity_start")])

    -- , "attract"

    , ( "read"
      , [("1.1", "Reading_activity")
        ,("1.2", "Grasp") -- "(Grasp <- understand.v)")
        ,("1.3", "Grasp") -- "(Grasp <- understand.v)")
        ,("1.4", "Reading_aloud")])

    , ( "like"
      , [("1.1", "Desiring")
        ,("1.2", "Experiencer_focus")])

    , ( "discuss"
      , [("1.1", "Discussion")])

    , ( "slow"
      , [("1.1", "Hindering")
        ,("1.2", "Hindering")])

    , ( "intend"
      , [("1.1", "Purpose")
        ,("1.2", "Purpose")])

    , ( "enter"
      , [("1.1", "Path_shape") -- "Path_shape || Arriving || Transition_to_a_situation || Becoming_a_member")
        ,("1.2", "Cause_to_be_included")])

    , ( "aim"
      , [("1.1", "Aiming")
        ,("1.2", "Purpose")])

    , ( "act"
      , [("1.1", "Performers_and_roles")
        ,("1.2", "Intentionally_act")])

    , ( "contend"
      , [("1.1", "Statement")
        ,("1.2", "Competition")]) -- "(Competition?)")])

    , ( "prove"
      , [("1.1", "Reasoning") -- "Reasoning || Evidence")
        ,("1.2", "Turning_out")
        ,("1.3", "Deserving")]) -- "(Deserving)")])

    , ( "elect"
      , [("1.1", "Change_of_leadership")
        ,("1.2", "Choosing")])

    , ( "determine"
      , [("1.1", "Becoming_aware") -- "(Becoming_aware <- learn.v)")
        ,("1.2", "Deciding")
        ,("1.3", "Contingency")])

    , ( "urge"
      , [("1.1", "Request")
        ,("1.2", "Attempt_suasion")])

    , ( "focus"
      , [("1.1", "Path_shape") -- "(Path_shape)")
        ,("1.2", "Attention")]) -- "(Attention)")])

    , ( "contribute"
      , [("1.1", "Giving")
        ,("1.2", "Relating_concepts")])

    , ( "choose"
      , [("1.1", "Choosing")
        ,("1.2", "Choosing")])

    , ( "learn"
      , [("1.1", "Becoming_aware")
        ,("1.2", "Becoming_aware")])

    , ( "release"
      , [("1.1", "Releasing")
        ,("1.2", "Publishing")
        ,("1.3", "Emitting")]) -- "(Emanating <- secrete.v, Emitting <- secrete.v)")])

    , ( "prepare"
      , [("1.1", "Activity_prepare")
        ,("1.2", "Activity_prepare")])

    , ( "worry"
      , [("1.1", "Emotion_active")])

    , ( "order"
      , [("1.1", "Request")
        ,("1.2", "Request_entity")])

    , ( "sit"
      , [("1.1", "Posture") -- "Posture || Placing")
        ,("1.2", "Being_located")
        ,("1.3", "Being_active") -- "(Being_active <- inactive.a)")
        ,("1.4", "Participation")
        ,("1.5", "Being_awake")
        ,("1.8", "none")])

    , ( "encourage"
      , [("1.1", "Attempt_suasion")
        ,("1.2", "Subjective_influence")])

    , ( "deliver"
      , [("1.1", "Creating")
        ,("1.4", "Intentionally_act")
        ,("1.5", "Delivery")
        ,("1.8", "none")])

    , ( "complain"
      , [("1.1", "Complaining")
        ,("1.2", "Judgment_communication")]) -- "(Judgment_communication <- charge.v)")])

    , ( "understand"
      , [("1.1", "Grasp")
        ,("1.2", "Awareness")
        ,("1.3", "Reasoning")])

    , ( "pull"
      , [("1.1", "Cause_motion")
        ,("1.2", "Cause_motion")
        ,("1.3", "Removing") -- "Removing || Activity_stop")
        ,("1.4", "Intentionally_act")
        ,("1.7", "Surviving") -- "(Surviving, Recovery)")
        ,("1.10", "idioms")])

    , ( "place"
      , [("1.1", "Placing")
        ,("1.4", "Arranging")
        ,("1.7", "Funding")]) -- "(Funding, Commercial_buy <- invest.v)")])

    , ( "jump"
      , [("1.1", "Self_motion")
        ,("1.6", "idiom")])

    , ( "compare"
      , [("1.1", "Evaluative_comparison")
        ,("1.2", "Evaluative_comparison")])

    , ( "finance"
      , [("1.1", "Funding")])

    , ( "eliminate"
      , [("1.1", "Removing")
        ,("1.4", "Beat_opponent")])

    , ( "draw"
      , [("1.1", "Cause_motion")
        ,("1.2", "Cause_motion")
        ,("1.3", "Cause_motion")
        ,("1.6", "Create_physical_artwork")
        ,("1.7", "Coming_up_with")
        ,("1.11", "idioms")])

    , ( "warn"
      , [("1.1", "Warning")])

    , ( "suffer"
      , [("1.1", "Eventive_affecting") -- "Catastrophe || Eventive_affecting")
        ,("1.2", "Improvement_or_decline")])

    , ( "publish"
      , [("1.1", "Publishing")])

    , ( "fight"
      , [("1.1", "Hostile_encounter")
        ,("1.2", "Seeking_to_achieve")
        ,("1.3", "Seeking_to_achieve")])

    , ( "earn"
      , [("1.1", "Earnings_and_losses")
        ,("1.2", "Deserving")]) -- "(Deserving)")])

    , ( "apply"
      , [("1.1", "Using")
        ,("1.2", "Being_relevant")
        ,("1.3", "Request") -- "(Request)")
        ,("1.4", "Using")])

    , ( "rule"
      , [("1.1", "Being_in_control") --  "Being_in_control  * Dominate_situation, Leadership")
        ,("1.2", "Verdict")
        ,("1.3", "Inclusion")])

    , ( "form"
      , [("1.1", "Coming_to_be") -- "Coming_to_be - Creating")
        ,("1.2", "Coming_to_be")
        ,("1.3", "Intentionally_create")
        ,("1.4", "Coming_to_be")
        ,("1.5", "none")])

    , ( "fill"
      , [("1.1", "Filling")
        ,("1.2", "Employing")
        ,("1.3", "Meet_specifications")
        ,("1.4", "Activity_finish") -- "(Activity_finish <- complete.v)")
        ,("1.5", "Explaining_the_facts")]) -- "(Explaining_the_facts <- explain.v )")])

    , ( "conclude"
      , [("1.1", "Activity_finish")
        ,("1.2", "Make_agreement_on_action")
        ,("1.3", "Coming_to_believe")
        ,("1.4", "Statement")])

    , ( "point"
      , [("1.1", "Adducing")
        ,("1.2", "Aiming")])

    , ( "pick"
      , [("1.1", "Choosing")
        ,("1.2", "Seeking")
        ,("1.7.1", "Placing") -- "(Placing)")
        ,("1.7.2", "Getting")
        ,("1.7.3", "Becoming_aware")
        ,("1.7.4", "Change_position_on_a_scale")  -- "Change_position_on_a_scale - Cause_change_of_position_on_a_scale")
        ,("1.7.6", "Choosing")
        ,("1.7.8", "Hit_or_miss")
        ,("1.7.10", "Personal_relationship")]) -- "(Personal_relationship <- date.v)")])

    , ( "describe"
      , [("1.1", "Communicate_categorization") -- "Communicate_categorization || (Statement || Explaining_the_fact)")
        ,("1.4", "Statement")])

    , ( "save"
      , [("1.1", "Rescuing")
        ,("1.2", "Storing")
        ,("1.3", "Frugality")]) -- "(Frugality <- waste.v)")])

    , ( "relate"
      , [("1.1", "Relating_concepts")
        ,("1.2", "Relating_concepts")])

    , ( "refuse"
      , [("1.1", "Agree_or_refuse_to_act") -- "Agree_or_refuse_to_act || Respond_to_request")
        ,("1.2", "Deny_or_grant_permission")])

    , ( "hurt"
      , [("1.2", "Eventive_affecting") --  "Eventive_affecting || Intentionally_affect")
        ,("1.3", "Experiencer_obj")
        ,("1.4", "Desiring")])

    , ( "hire"
      , [("1.1", "Hiring")])

    , ( "die"
      , [("1.1", "Ceasing_to_be")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.4", "Being_operational")]) -- "(Being_operational <- broken.a)")])

    , ( "confirm"
      , [("1.1", "Evidence")
        ,("1.2", "Statement")
        ,("1.3", "Verification")])

    , ( "fear"
      , [("1.2", "Experiencer_focus")])

    , ( "negotiate"
      , [("1.1", "Discussion")])

    , ( "deal"
      , [("1.1", "Resolve_problem")
        ,("1.2", "Forming_relationships") -- "(Forming_relationships) || (Thriving)")
        ,("1.3", "Topic")
        ,("1.4", "Dispersal")])

    , ( "account"
      , [("1.1", "Justifying")
        ,("1.2", "Amounting_to")]) -- "(Amounting_to)")])

    , ( "lower"
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2", "Cause_change_of_position_on_a_scale")])

    , ( "block"
      , [("1.1", "Hindering")
        ,("1.2", "Hindering")])

    , ( "adopt"
      , [("1.1", "Adopt_selection")
        ,("1.2", "Adopt_selection")
        ,("1.3", "Forming_relationships")])

    , ( "impose"
      , [("1.1", "Imposing_obligation")])

    , ( "exceed"
      , [("1.1", "Surpassing")])

    , ( "contain"
      , [("1.1", "Containing") -- "Containing || Inclusion  || (Membership)")
        ,("1.2", "Limiting")])

    , ( "allege"
      , [("1.1", "Statement")])

    , ( "tend"
      , [("1.1", "Likelihood")
        ,("1.2", "Assistance")])

    , ( "retain"
      , [("1.1", "Retaining")
        ,("1.2", "Retaining")
        ,("1.3", "Employing")])

    , ( "promise"
      , [("1.1", "Commitment")
        ,("1.2", "Omen")])

    , ( "occur"
      , [("1.1", "Event")])

    , ( "accuse"
      , [("1.1", "Judgment_communication")])

    , ( "threaten"
      , [("1.1", "Commitment")
        ,("1.2", "Omen")])

    , ( "identify"
      , [("1.1", "Verification")
        ,("1.2", "Cognitive_connection")])

    , ( "combine"
      , [("1.1", "Cause_to_amalgamate")
        ,("1.2", "Amalgamation")])

    , ( "throw"
      , [("1.1", "Body_movement")
        ,("1.2", "Abandonment") -- "(Abandonment) || (Removing) || (Process_stop) || (Agree_or_refuse_to_act)")
        ,("1.5", "Experiencer_obj")
        ,("1.6", "Arranging")]) --  "(Arranging <- set up.v)")])

    , ( "range"
      , [("1.1", "Delimitation_of_diversity")])

    , ( "declare"
      , [("1.1", "Statement")])

    , ( "admit"
      , [("1.1", "Affirm_or_deny")
        ,("1.2", "Institutionalization")])

    , ( "catch"
      , [("1.1", "Taking_captive") -- "Taking_captive || Experience_obj")
        ,("1.2", "Taking_captive") -- "Taking_captive || Perception_active")
        ,("1.4", "Accomplishment") -- "(Accomplishment)")
        ,("1.5", "Come_down_with") -- "Come_down_with || Catching_fire")
        ,("1.6", "Attending")
        ,("1.7", "Process_start")
        ,("1.8", "Getting")])

    , ( "attempt"
      , [("1.1", "Attempt")])

    , ( "adjust"
      , [("1.1", "Adjusting")
        ,("1.2", "Adopt_selection")]) -- "(Adopting_selection)")])

    , ( "speak"
      , [("1.1", "Communication")
        ,("1.2", "Speak_on_topic")
        ,("1.4", "Leadership")
        ,("1.5", "Communication")]) -- "(Communication)")])

    , ( "list"
      , [("1.1", "Categorization") --  "(Categorization)")
        ,("1.2", "Change_posture")
        ,("1.3", "none")])

    , ( "extend"
      , [("1.2", "Change_event_duration") -- "Change_event_duration || (Change_position_on_a_scale)")
        ,("1.3", "Offering")])

    , ( "respond"
      , [("1.1", "Response")
        ,("1.2", "Communication_response")])

    , ( "remove"
      , [("1.1", "Removing")])

    , ( "recover"
      , [("1.1", "Recovery")
        ,("1.2", "Rejuvenation")
        ,("1.3", "Reparation")]) -- "(Reparation) || (Rejuvenation)")])

    , ( "realize"
      , [("1.1", "Coming_to_believe")
        ,("1.2", "Accomplishment") -- "(Accomplishment)")
        ,("1.3", "Earnings_and_losses")])

    , ( "fly"
      , [("1.1", "Motion") -- "Motion - Cause_motion")
        ,("1.5", "Success_or_failure")])

    , ( "view"
      , [("1.1", "Categorization")
        ,("1.2", "Perception_active")])

    , ( "study"
      , [("1.1", "Scrutiny")
        ,("1.2", "Studying")])

    , ( "recall"
      , [("1.1", "Remembering_experience") -- "Remembering_experience || Remembering_information")
        ,("1.2", "Process_stop")]) -- "(Process_stop?)")])

    , ( "discover"
      , [("1.1", "Becoming_aware")])

    , ( "compete"
      , [("1.1", "Competition")])

    , ( "guarantee"
      , [("1.1", "Commitment") -- "(Commitment <- promise.v)")
        ,("1.2", "Evidence")])

    , ( "generate"
      , [("1.1", "Cause_to_start")
        ,("1.2", "Building")]) --   "(Building) || (Creating)")])

    , ( "refer"
      , [("1.2", "Topic") -- "(Topic) || (Mention)")
        ,("1.3", "Telling")]) -- "(Telling)")])

    , ( "pursue"
      , [("1.1", "Seeking_to_achieve") -- "Seeking_to_acheive || Seeking")
        ,("1.2", "Seeking")])

    , ( "offset"
      , [("1.1", "Evaluative_comparison")])

    , ( "defend"
      , [("1.1", "Defending")
        ,("1.2", "Justifying")])

    , ( "conduct"
      , [("1.1", "Performers_and_roles")
        ,("1.2", "Control")
        ,("1.4", "Transfer")])

    , ( "supply"
      , [("1.1", "Supply")])

    , ( "retire"
      , [("1.1", "Quitting")
        ,("1.3", "Process_stop")
        ,("1.6", "Removing")]) --  "(Remove?)")])

    , ( "permit"
      , [("1.1", "Preventing_or_letting")])

    , ( "emerge"
      , [("1.1", "Departing")
        ,("1.2", "Coming_to_be")])

    , ( "damage"
      , [("1.1", "Damaging")])

    , ( "concern"
      , [("1.1", "Being_relevant")
        ,("1.2", "Experiencer_obj")]) --  "(Experiencer_obj <- worry.v) || (Emotion_active <- worry.v)")])

    , ( "violate"
      , [("1.1", "Compliance")
        ,("1.2", "Damaging")])

    , ( "resume"
      , [("1.1", "Activity_resume") -- "Activity_resume || Cause_to_resume")
        ,("1.2", "Process_resume")])

    , ( "match"
      , [("1.1", "Evaluative_comparison")
        ,("1.2", "Funding")
        ,("1.3", "Choosing") -- "(Choosing)")
        ,("1.4", "Competition")])

    , ( "value"
      , [("1.1", "Assessing")
        ,("1.2", "Judgment")])

    , ( "suspend"
      , [("1.2", "Activity_pause") -- "(Activity_pause)")
        ,("1.3", "Activity_pause")])

    , ( "stem"
      , [("1.1", "Origin")
        ,("1.3", "Hindering")
        ,("1.5", "Resolve_problem")]) -- "(Resolving_problem <- deal.v)")])

    , ( "promote"
      , [("1.1", "Cause_to_make_progress") -- "(Cause_to_make_progress) | (Cause_to_expand)")
        ,("1.2", "Cause_change_of_position_on_a_scale")]) --  "(Cause_change_of_position_on_a_scale)")])

    , ( "market"
      , [("1.1", "Commerce_sell")]) -- "(Commerce_sell)")])

    , ( "prompt"
      , [("1.1", "Cause_to_start")])

    , ( "ignore"
      , [("1.1", "Regard")]) --  "Regard || Attention")])

    , ( "distribute"
      , [("1.1", "Dispersal")
        ,("1.3", "Publishing")
        ,("1.4", "Arranging")])

    , ( "benefit"
      , [("1.1", "Conferring_benefit")
        ,("1.2", "Funding")])

    , ( "advise"
      , [("1.1", "Telling")
        ,("1.2", "Telling")])

    , ( "acknowledge"
      , [("1.1", "Affirm_or_deny")])

    , ( "soar"
      , [("1.2", "Change_position_on_a_scale")
        ,("1.3", "Improvement_or_decline")])

    , ( "recognize"
      , [("1.1", "Becoming_aware")
        ,("1.2", "Regard")
        ,("1.3", "Compliance")])

    , ( "climb"
      , [("1.1", "Self_motion")])

    , ( "blame"
      , [("1.1", "Judgment_communication")
        ,("1.2", "Judgment")])

    , ( "ban"
      , [("1.1", "Prohibiting_or_licensing")
        ,("1.2", "Exclude_member")])

    , ( "anticipate"
      , [("1.1", "Expectation")
        ,("1.3", "Relative_time")])

    , ( "recommend"
      , [("1.1", "Attempt_suasion")
        ,("1.2", "Experiencer_obj")])

    , ( "obtain"
      , [("1.1", "Getting")])

    , ( "monitor"
      , [("1.1", "Scrutiny")])

    , ( "express"
      , [("1.1", "Statement")])

    , ( "demand"
      , [("1.1", "Request")
        ,("1.2", "Have_as_requirement")])

    , ( "bear"
      , [("1.1", "copula") -- "(copula)")
        ,("1.2", "Giving_birth")
        ,("1.3", "Creating")
        ,("1.5", "Tolerating")])

    , ( "strike"
      , [("1.1", "Attack") -- "Attack || Impact")
        ,("1.2", "Cognitive_impact")
        ,("1.4", "Accomplishment")
        ,("1.5", "Political_actions")
        ,("1.8", "Activity_start")
        ,("1.10", "idioms")])

    , ( "share"
      , [("1.1", "Commonality")
        ,("1.2", "Sharing")
        ,("1.3", "Dispersal")])

    , ( "plunge"
      , [("1.1", "Causation")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.3", "Activity_start")])

    , ( "depend"
      , [("1.1", "Contingency")
        ,("1.2", "Reliance")])

    , ( "concede"
      , [("1.1", "Affirm_or_deny")
        ,("1.2", "Surrendering_possession")])

    , ( "trigger"
      , [("1.1", "Cause_to_start")])

    , ( "miss"
      , [("1.1", "Hit_or_miss")
        ,("1.2", "Perception_experience")
        ,("1.3", "Possession") -- "(Possesion)")
        ,("1.4", "Experiencer_focus")])

    , ( "measure"
      , [("1.1", "Assessing")
        ,("1.2", "Assessing")
        ,("1.3", "Dimension")
        ,("1.5", "Meet_specifications")])

    , ( "employ"
      , [("1.1", "Employing")
        ,("1.2", "Using")])

    , ( "wear"
      , [("1.1", "Wearing")
        ,("1.2", "Improvement_or_decline")])

    , ( "visit"
      , [("1.1", "Visiting")])

    , ( "present"
      , [("1.1", "Cause_to_perceive") -- "Cause_to_perceive || (Publishing)")
        ,("1.2", "Giving")
        ,("1.3", "Causation")
        ,("1.7", "Communication")])

    , ( "prefer"
      , [("1.1", "Preference")])

    , ( "spread"
      , [("1.1", "Expansion")
        ,("1.2", "Dispersal")])

    , ( "resolve"
      , [("1.1", "Deciding")
        ,("1.2", "Resolve_problem")
        ,("1.3", "Commitment")])

    , ( "press"
      , [("1.1", "Cause_motion")
        ,("1.2", "Cause_to_continue")
        ,("1.3", "Attempt_suasion")
        ,("1.5", "Request") -- "(Request <- petition.n)")
        ,("1.8", "Cause_change")
        ,("1.10", "none")])

    , ( "perform"
      , [("1.1", "Intentionally_act")
        ,("1.2", "Performers_and_roles")
        ,("1.3", "none")])

    , ( "favor"
      , [("1.1", "Partiality")])

    , ( "convert"
      , [("1.1", "Undergo_change") -- "Undergo_change | Undergo_transformation - Cause_change   | Exchange_currency")
        ,("1.2", "Cause_change")])

    , ( "borrow"
      , [("1.1", "Borrowing")
        ,("1.2", "Adopt_selection")]) -- "(Adopt_selection)")])

    , ( "restrict"
      , [("1.1", "Limiting")])

    , ( "restore"
      , [("1.1", "Transition_to_state")]) --  "(Transition_to_state - Cause_change) || (Rejuvenation)")])

    , ( "participate"
      , [("1.1", "Participation")])

    , ( "finish"
      , [("1.1", "Activity_finish")
        ,("1.2", "Transition_to_state")])

    , ( "ensure"
      , [("1.1", "Verification")])

    , ( "abandon"
      , [("1.1", "Abandonment")])

    , ( "mark"
      , [("1.2", "Cause_to_perceive")
        ,("1.3", "Cause_to_perceive")
        ,("1.4", "Recording")
        ,("1.7", "Statement")
        ,("1.8", "Cause_change_of_position_on_a_scale")
        ,("1.9", "Cause_change_of_position_on_a_scale")])

    , ( "link"
      , [("1.1", "Relating_concepts")
        ,("1.2", "Attaching")])

    , ( "feature"
      , [("1.1", "Performers_and_roles")])

    , ( "delay"
      , [("1.1", "Hindering")
        ,("1.2", "Change_event_time")])


    , ( "treat"
      , [("1.1", "Treating_and_mistreating")
        ,("1.3", "Processing_materials")
        ,("1.4", "Medical_intervention")])

    , ( "survive"
      , [("1.1", "Surviving")])

    , ( "lie"
      , [("1.1", "Being_located") -- "Posture || Being_located")
        ,("1.2", "Prevarication")])

    , ( "figure"
      , [("1.1", "Reasoning")
        ,("1.2", "Resolve_problem")
        ,("1.3", "Function")])

    , ( "count"
      , [("1.1", "Categorization")  -- Categorization
        ,("1.2", "Importance")
        ,("1.3", "Categorization")
        ,("1.4", "Reliance")])

    , ( "bid"
      , [("1.1", "Commerce_scenario")])  --  Commercial_scenario

    , ( "advance"
      , [("1.1", "Self_motion") -- "Self_motion || Cause_motion")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.3", "Cause_to_make_progress")
        ,("1.4", "Cause_to_make_progress") -- "(Cause_to_make_progress <- promote.v)")
        ,("1.5", "Lending")
        ,("1.6", "Change_event_time")])

    , ( "test"
      , [("1.1", "Examination") -- "Examination || Assessing")
        ,("1.2", "Operational_testing")])

    , ( "step"
      , [("1.1", "Self_motion") -- "Self_motion || Transition_to_a_state")
        ,("1.3", "Cause_motion")
        ,("1.10", "Cause_change_of_position_on_a_scale") -- "Cause_change_position_on_a_scale || Cause_change_of_strength")
        ,("1.11", "none")])

    , ( "attribute"
      , [("1.1", "Relating_concepts")]) -- "(Relating_concepts) || (Cognitive_connection)")])

    , ( "walk"
      , [("1.1", "Self_motion")
        ,("1.4", "Quitting")])

    , ( "steal"
      , [("1.1", "Theft")
        ,("1.2", "Self_motion")
        ,("1.3", "Successful_action")])

    , ( "investigate"
      , [("1.1", "Scrutiny")]) -- "Research || Scrutiny || Criminal_investigation")])

    , ( "enjoy"
      , [("1.1", "Experiencer_focus")
        ,("1.2", "Conferring_benefit")])

    , ( "clear"
      , [("1.1", "Emptying") -- "Emptying || Removing")
        ,("1.2", "Improvement_or_decline")
        ,("1.3", "Deciding")    -- (Deciding)
        ,("1.4", "Evading")   -- (Evading)
        ,("1.6", "Quitting_a_place")])

    , ( "achieve"
      , [("1.1", "Accomplishment")])

    , ( "signal"
      , [("1.1", "Gesture")
        ,("1.2", "Sign")])

    , ( "renew"
      , [("1.1", "Activity_resume")
        ,("1.2", "Improvement_or_decline")]) -- "(Cause_to_make_progress) || (Improvement_or_decline)")])

    , ( "mature"
      , [("1.1", "Ontogeny") -- "Ontogeny || Cause_to_make_progress")
        ,("1.3", "Process_end")])

    , ( "exclude"
      , [("1.1", "Inclusion")]) -- "Inclusion || Preventing_or_letting")])

    , ( "engage"
      , [("1.1", "Participation")])

    , ( "concentrate"
      , [("1.1", "Perception_active") -- "Perception_active || (Attention)")])
        ,("1.2", "Distributed_position")])

    , ( "award"
      , [("1.1", "Rewards_and_punishments")])

    , ( "attend"
      , [("1.1", "Attending")
        ,("1.4", "Perception_active")])

    , ( "surge"
      , [("1.1", "Motion")
        ,("1.2", "Change_position_on_a_scale")])

    , ( "slip"
      , [("1.1", "Self_motion")
        ,("1.2", "Cause_motion") -- "Cause_motion || Motion")
        ,("1.3", "Change_position_on_a_scale") -- "Change_position_on_a_scale || Improvement_or_decline")
        ,("1.4", "Successful_action")])

    , ( "shift"
      , [("1.1", "Cause_change")
        ,("1.2", "Self_motion")
        ,("1.3", "Cause_change")])

    , ( "shake"
      , [("1.1", "Cause_motion") -- "Motion || Cause_motion")
        ,("1.2", "Subversion") -- "Subversion <- undermine.v")
        ,("1.3", "Gesture")
        ,("1.4", "Removing")])

    , ( "rely"
      , [("1.1", "Reliance")])

    , ( "execute"
      , [("1.1", "Execution")
        ,("1.2", "Execute_plan")])

    , ( "cancel"
      , [("1.1", "Activity_stop")
        ,("1.2", "Activity_stop")])

    , ( "sustain"
      , [("1.1", "Retaining") -- "Support || Retaining")
        ,("1.2", "Experiencer_focus")]) --  "Experiencer_focus <- suffer.v")])

    , ( "remember"
      , [("1.1", "Remembering_experience") -- "Remembering_experience || Remembering_information")
        ,("1.2", "Memory")])

    , ( "record"
      , [("1.1", "Recording")
        ,("1.2", "Evaluative_comparison")]) -- "(Evaluative_comparison <- measure.v)")])

    , ( "project"
      , [("1.1", "Communication")
        ,("1.5", "Estimating")])

    , ( "fund"
      , [("1.1", "Funding")
        ,("1.3", "none")])

    , ( "expire"
      , [("1.1", "Process_end")])

    , ( "enable"
      , [("1.1", "Preventing_or_letting")])

    , ( "travel"
      , [("1.1", "Travel")
        ,("1.2", "Motion")])

    , ( "transfer"
      , [("1.1", "Transfer")
        ,("1.2", "Transfer")
        ,("1.3", "none")])

    , ( "tie"
      , [("1.1", "Attaching")
        ,("1.2", "Make_cognitive_connection")
        ,("1.3", "Competition")])

    , ( "suppose"
      , [("1.1", "Conditional_occurrence")
        ,("1.2", "Opinion") -- "Opinion || (Likelihood)")
        ,("1.6", "none")])

    , ( "select"
      , [("1.1", "Choosing")])

    , ( "insure"
      , [("1.1", "Commitment") -- "Commitment <- guarantee.v")
        ,("1.2", "Commitment")])

    , ( "halt"
      , [("1.1", "Activity_stop")])

    , ( "exercise"
      , [("1.1", "Using")
        ,("1.2", "Exercising")])

    , ( "weaken"
      , [("1.1", "Cause_change_of_strength")])

    , ( "teach"
      , [("1.1", "Education_teaching")])

    , ( "review"
      , [("1.1", "Scrutiny")
        ,("1.2", "Assessing")])  -- "(Assessing)")])

    , ( "manufacture"
      , [("1.1", "Manufacturing")])

    , ( "lack"
      , [("1.1", "Have_as_requirement")]) -- "Have_as_requirement || Needing")])

    , ( "double"
      , [("1.1", "Change_position_on_a_scale")
        ,("1.4", "Function")])

    , ( "collapse"
      , [("1.1", "Breaking_apart") -- "Breaking_apart || Cause_to_fragment")
        ,("1.2", "Improvement_or_decline")])

    , ( "address"
      , [("1.1", "Speak_on_topic")
        ,("1.4", "Topic")])

    , ( "submit"
      , [("1.2", "Submitting_documents")])

    , ( "owe"
      , [("1.1", "Borrowing")
        ,("1.2", "Being_obligated")
        ,("1.3", "Relating_concepts")])

    , ( "import"
      , [("1.1", "Importing")])

    , ( "criticize"
      , [("1.1", "Judgment_communication")])

    , ( "appoint"
      , [("1.1", "Appointing")])

    , ( "vary"
      , [("1.1", "Delimitation_of_diversity") -- "Diversity || Delimitation_of_diversity")
        ,("1.2", "Diversity")])

    , ( "state"
      , [("1.1", "Statement")])

    , ( "sound"
      , [("1.1", "Give_impression")
        ,("1.2", "Make_noise")])

    , ( "solve"
      , [("1.1", "Resolve_problem")
        ,("1.2", "Resolve_problem")])

    , ( "ship"
      , [("1.1", "Sending")])

    , ( "mention"
      , [("1.1", "Mention")])

    , ( "lay"
      , [("1.1", "Placing")
        ,("1.2", "Arranging") -- "Arranging || Activity_prepare")
        ,("1.4", "Imposing_obligation") -- "Imposing_obligation || Judgment_communication")
        ,("1.5.4", "Activity_stop")
        ,("1.5.5", "Being_employed")
        ,("1.5.7", "Using_resource")
        ,("1.5.11", "idioms")
        ,("1.5.12", "none")])

    , ( "last"
      , [("1.2", "Duration_relation")])

    , ( "dump"
      , [("1.1", "Cause_motion")
        ,("1.2", "Removing")
        ,("1.4", "none")])

    , ( "divide"
      , [("1.1", "Separating")
        ,("1.3", "Be_in_agreement_on_assessment")])

    , ( "convict"
      , [("1.1", "Verdict")])

    , ( "commit"
      , [("1.1", "Committing_crime")
        ,("1.2", "Commitment")])

    , ( "approach"
      , [("1.1", "Arriving")
        ,("1.2", "Attempt_suasion")
        ,("1.3", "Attempting_and_resolving_scenario")])

    , ( "withdraw"
      , [("1.1", "Quitting_a_place") -- "Quitting_a_place || Withdraw_from_participation || Activity_stop")
        ,("1.2", "Removing")
        ,("1.3", "Taking")])

    , ( "target"
      , [("1.1", "Aiming")])

    , ( "rebound"
      , [("1.2", "Recovery")])

    , ( "react"
      , [("1.1", "Response")])

    , ( "question"
      , [("1.1", "Questioning")
        ,("1.2", "Certainty")])

    , ( "mount"
      , [("1.1", "Installing")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.3", "Making_arrangements")
        ,("1.4", "Placing")
        ,("1.5", "Intentional_traversing")])

    , ( "install"
      , [("1.1", "Installing")])

    , ( "dismiss"
      , [("1.1", "Respond_to_proposal") -- "Respond_to_proposal || Judgment")
        ,("1.2", "Activity_finish") -- "Activity_finish || Firing")
        ,("1.3", "Activity_finish")])

    , ( "challenge"
      , [("1.1", "Compliance") -- "(Compliance)")
        ,("1.2", "Competition")
        ,("1.3", "Cause_to_perceive")]) -- "(Cause_to_perceive)")])

    , ( "associate"
      , [("1.1", "Make_cognitive_connection")
        ,("1.2", "Forming_relationships")])

    , ( "track"
      , [("1.2", "Perception_active")
        ,("1.3", "Traversing")
        ,("1.4", "Locating")])

    , ( "switch"
      , [("1.1", "Replacing")
        ,("1.2", "Replacing")
        ,("1.3", "Change_operational_state")])

    , ( "sue"
      , [("1.1", "Judgment_communication")]) -- "(Judgment_communication)")])

    , ( "shoot"
      , [("1.1", "Hit_target") --  "Hit_target || Shoot_projectiles")
        ,("1.2", "Motion")
        ,("1.3", "Intentionally_create")])  -- (Intentionally_create)

    , ( "preserve"
      , [("1.1", "Preserving")
        ,("1.2", "Protecting")])

    , ( "plead"
      , [("1.1", "Request")
        ,("1.3", "Entering_of_plea")
        ,("1.4", "none")])

    , ( "enact"
      , [("1.1", "Change_operational_state")])  -- (Change_operational_state)

    , ( "dominate"
      , [("1.1", "Dominate_situation")]) -- "Dominate_situation || Being_in_control || Dominate_competitor")])

    , ( "attach"
      , [("1.1", "Attaching")
        ,("1.2", "Emotion_active" )])  -- "(Emotion_active) || (Experiencer_focus)")])

    , ( "appeal"
      , [("1.1", "Appeal")
        ,("1.2", "Request")
        ,("1.3", "Cause_emotion")])

    , ( "wonder"
      , [("1.1", "Cogitation")])

    , ( "struggle"
      , [("1.1", "Hostile_encounter")
        ,("1.2", "Seeking_to_achieve")])

    , ( "stabilize"
      , [("1.1", "Cause_fluidic_motion") -- "(Cause_fluidic_motion) || (Cause_motion) || (Cause_change)")
        ,("1.2", "Change_operational_state")]) -- "(Change_operational_state) || (Cause_motion) || (Cause_change)")])

    , ( "shut"
      , [("1.1", "Closure")
        ,("1.2", "Supply")
        ,("1.4", "Inclusion") -- "Inclusion <- exclude.v")
        ,("1.5", "Activity_stop") -- "Activity_stop || Cause_to_end")
        ,("1.6", "Activity_stop")])

    , ( "roll"
      , [("1.1", "Motion") -- "Motion || Cause_motion || Moving_in_place")
        ,("1.2", "Placing") -- "Placing || Path_shape")
        ,("1.7", "Activity_start") --  "(Activity_start)")
        ,("1.10", "Activity_resume")]) -- "(Activity_resume)")])

    , ( "restructure"
      , [("1.1", "Reforming_a_system")])

    , ( "process"
      , [("1.1", "Processing_materials")
        ,("1.3", "Intentionally_affect")  -- "(Intentionally_affect)")
        ,("1.4", "Grasp")]) -- "(Grasp)")])

    , ( "oversee"
      , [("1.1", "Being_in_control")])

    , ( "experience"
      , [("1.1", "Undergoing")]) -- "Undergoing || Perception_experience || Feeling")])

    , ( "arrive"
      , [("1.1", "Arriving")])

    , ( "widen"
      , [("1.1", "Cause_expansion")])

    , ( "strengthen"
      , [("1.1", "Cause_change_of_strength")
        ,("1.2", "Cause_change_of_strength")])

    , ( "rush"
      , [("1.1", "Self_motion")
        ,("1.2", "Self_motion")]) -- "Self_motion || (Change_operational_state)")])

    , ( "revive"
      , [("1.1", "Cause_to_resume")])

    , ( "revise"
      , [("1.1", "Cause_change")])

    , ( "register"
      , [("1.1", "Becoming_a_member") -- "Becoming_a_member || Recording")
        ,("1.2", "Becoming_aware")
        ,("1.3", "Assessing")])

    , ( "persuade"
      , [("1.1", "Suasion")])

    , ( "grant"
      , [("1.1", "Deny_or_grant_permission")])

    , ( "consist"
      , [("1.1", "Partitive") -- "(Partitive) || (Being_in_category)")
        ,("1.2", "Partitive")]) -- "(Partitive)")])

    , ( "collect"
      , [("1.1", "Gathering_up")
        ,("1.2", "Commerce_collect")])

    , ( "beat"
      , [("1.1", "Beat_opponent")
        ,("1.2", "Cause_motion") -- "Cause_move_in_place || Cause_to_make_noise")
        ,("1.3", "Cause_harm")
        ,("1.11.3", "Cause_motion")
        ,("1.11.9", "none")])

    , ( "assert"
      , [("1.1", "Statement")
        ,("1.2", "Claim_ownership")
        ,("1.3", "Statement")])

    , ( "unveil"
      , [("1.1", "Reveal_secret")]) -- "Reveal_secret || Evidence")])

    , ( "survey"
      , [("1.1", "Scrutiny")
        ,("1.2", "Assessing") -- "Assessment || Scrutiny")
        ,("1.3", "Assessing")]) -- "(Assessment)")])

    , ( "spur"
      , [("1.1", "Subjective_influence")])

    , ( "seize"
      , [("1.1", "Taking")
        ,("1.2", "Getting") -- "(Getting) || (Using) || (Perception_active)")
        ,("1.3", "Taking_captive")
        ,("1.5", "Activity_stop")])

    , ( "reopen"
      , [("1.1", "Activity_resume")])

    , ( "regulate"
      , [("1.2", "Control")])

    , ( "proceed"
      , [("1.1", "Cause_to_continue")
        ,("1.3", "Cause_to_continue")])

    , ( "narrow"
      , [("1.1", "Limiting")
        ,("1.2", "Cause_expansion")])

    , ( "lift"
      , [("1.1", "Cause_change_of_position_on_a_scale") -- "Cause_change_of_position_on_a_scale || Placing")
        ,("1.3", "Removing")
        ,("1.6", "Experiencer_obj")
        ,("1.7", "Removing")])

    , ( "invite"
      , [("1.2", "Request")
        ,("1.3", "Suasion")])

    , ( "fuel"
      , [("1.1", "Supply")
        ,("1.2", "Cause_to_start")])

    , ( "fix"
      , [("1.1", "Resolve_problem")
        ,("1.3", "Deciding")
        ,("1.4", "Activity_prepare")])

    , ( "fine"
      , [("1.1", "Fining")])

    , ( "cast"
      , [("1.1", "Cause_motion") -- "Cause_motion || Body_movement")
        ,("1.2", "Create_physical_artwork")
        ,("1.3", "Performers_and_roles")
        ,("1.4", "Choosing")
        ,("1.5", "Motion")])

    , ( "bar"
      , [("1.1", "Preventing_or_letting")
        ,("1.2", "Preventing_or_letting")])

    , ( "veto"
      , [("1.1", "Preventing_or_letting")]) -- "Preventing_or_letting || Respond_to_proposal || Agree_or_refuse_to_act")])

    , ( "testify"
      , [("1.1", "Speak_on_topic") -- "(Speak_on_topic) || (Communication) || (Statement)")
        ,("1.2", "Evidence")])

    , ( "stress"
      , [("1.1", "Emphasizing")])

    , ( "request"
      , [("1.1", "Request")])

    , ( "merge"
      , [("1.1", "Amalgamation") -- "Amalgamation || Cause_to_amalgamate")
        ,("1.2", "Amalgamation")])

    , ( "justify"
      , [("1.1", "Justifying")
        ,("1.2", "Justifying")])

    , ( "destroy"
      , [("1.1", "Destroying")
        ,("1.2", "Beat_opponent")])

    , ( "curb"
      , [("1.1", "Control")])

    , ( "calculate"
      , [("1.1", "Assessing") -- "(Assessing) || (Evaluative Comparison)")
        ,("1.2", "Judgment") -- "Judgment || Assessing")
        ,("1.3", "Purpose")])

    , ( "arrange"
      , [("1.1", "Arranging")
        ,("1.2", "Making_arrangements")
        ,("1.3", "Intentionally_create")])

    , ( "accompany"
      , [("1.1", "Cotheme")
        ,("1.2", "Partitive") -- "(Partitive) || (Rest)")
        ,("1.3", "Performers_and_roles")])

    , ( "specialize"
      , [("1.1", "Expertise")])

    , ( "repeat"
      , [("1.1", "Event_instance")
        ,("1.2", "Intentionally_act")]) -- "(Intentionally_act)")])

    , ( "rally"
      , [("1.1", "Gathering_up")
        ,("1.2", "Recovery")]) -- "Recovery || (Transition_to_state)")])

    , ( "prohibit"
      , [("1.1", "Prohibiting_or_licensing")])

    , ( "knock"
      , [("1.1", "Impact")
        ,("1.2", "Make_noise")
        ,("1.5", "Cause_harm")  -- (Cause_harm)
        ,("1.6", "Impact")   -- (Impact)
        ,("1.8", "Cause_change_of_position_on_a_scale")
        ,("1.9", "Theft")])

    , ( "gather"
      , [("1.1", "Gathering_up")
        ,("1.2", "Assemble")
        ,("1.3", "Cause_change_of_position_on_a_scale")])

    , ( "eat"
      , [("1.1", "Ingestion")
        ,("1.3", "Subversion") -- "Subversion || Rotting")
        ,("1.4", "Responsibility")]) -- "(Responsbility)")])

    , ( "bolster"
      , [("1.1", "Supporting")
        ,("1.2", "Placing")])

    , ( "average"
      , [("1.1", "Amounting_to")])

    , ( "assist"
      , [("1.1", "Assistance")
        ,("1.2", "Assistance")])

    , ( "tumble"
      , [("1.1", "Change_position_on_a_scale")])

    , ( "stick"
      , [("1.1", "Being_attached" ) -- "Being_attached || Attaching")
        ,("1.2", "Attaching")
        ,("1.3", "Activity_ongoing")
        ,("1.4", "Causation")]) -- "(Causation)")])

    , ( "resist"
      , [("1.1", "Repel")
        ,("1.2", "Respond_to_proposal")])

    , ( "plummet"
      , [("1.1", "Motion_directional")]) -- "Motion_directional || Change_position_on_a_scale")])

    , ( "listen"
      , [("1.1", "Perception_active")])

    , ( "lend"
      , [("1.1", "Cause_change") --  "(Cause_change) || (Transition_to_a_quality)")
        ,("1.2", "Lending")
        ,("1.3", "Suitability")
        ,("1.4", "Assistance")])

    , ( "forget"
      , [("1.1", "Remembering_information") -- "Remembering_information || Rememberging_to_do || Remembering_experience || Memory")
        ,("1.3", "Cause_to_perceive")]) -- "(Cause_to_perceive) || (Activity_end)")])

    , ( "enhance"
      , [("1.1", "Improvement_or_decline")])

    , ( "clean"
      , [("1.1", "Removing") -- "Removing || Improvement_or_decline")
        ,("1.5.6", "Arranging")
        ,("1.5.8", "Improvement_or_decline")])

    , ( "care"
      , [("1.1", "Experiencer_focus")                      -- (Experiencer_focus)
        ,("1.2", "Assistance") -- (Assistance) || (Medical_intervention)
        ,("1.3", "Desiring")])

    , ( "cap"
      , [("1.2", "Limiting")
        ,("1.4", "Activity_finish")])

    , ( "afford"
      , [("1.1", "Tolerating")
        ,("1.2", "Tolerating")       -- (Tolerating) || (Supply)
        ,("1.3", "Supply")])

    , ( "absorb"
      , [("1.1", "Soaking_up")
        ,("1.2", "Grasp")
        ,("1.3", "Amalgamation")
        ,("1.4", "Experiencer_obj")
        ,("1.5", "Impact")])

    , ( "repair"
      , [("1.1", "Resolve_problem")])

    , ( "pour"
      , [("1.1", "Fluidic_motion")
        ,("1.2", "Mass_motion")    -- Mass_motion || Fluidic_motion
        ,("1.3", "Using_resource")]) -- Using_resource || Supply

    , ( "lease"
      , [("1.1", "Renting")
        ,("1.2", "Renting_out")])

    , ( "imply"
      , [("1.1", "Evidence")
        ,("1.2", "Communication")])

    , ( "found"
      , [("1.1", "Intentionally_create")])

    , ( "feed"
      , [("1.1", "Ingestion")
        ,("1.2", "Supply")
        ,("1.3", "Capacity")
        ,("1.5", "Ingestion")
        ,("1.6", "Path_shape")
        ,("1.8", "Emotion_directed")])

    , ( "define"
      , [("1.1", "Communicate_categorization")
        ,("1.2", "Categorization")])

    , ( "belong"
      , [("1.1", "Possession")
        ,("1.2", "Membership")])

    , ( "alter"
      , [("1.1", "Cause_change")])

    , ( "weigh"
      , [("1.1", "Dimension")
        ,("1.3", "Assessing")
        ,("1.4", "Importance")
        ,("1.7", "Participation")])

    , ( "surround"
      , [("1.1", "Surrounding")])

    , ( "strip"
      , [("1.1", "Removing")      -- Removing || Taking || Emptying
        ,("1.3", "Wearing")])

    , ( "split"
      , [("1.1", "Separating")
        ,("1.2", "Breaking_apart")
        ,("1.3", "Personal_relationship")
        ,("1.4", "Departing")])

    , ( "speed"
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2", "Cause_change_of_position_on_a_scale")])

    , ( "regard"
      , [("1.1", "Categorization")
        ,("1.3", "Topic")])

    , ( "organize"
      , [("1.1", "Intentionally_create")
        ,("1.2", "Making_arrangements")])

    , ( "locate"
      , [("1.1", "Locating")
        ,("1.2", "Being_located")])

    , ( "interpret"
      , [("1.1", "Categorization")])

    , ( "inform"
      , [("1.1", "Telling")])

    , ( "forecast"
      , [("1.1", "Predicting")])

    , ( "export"
      , [("1.1", "Exporting")])

    , ( "demonstrate"
      , [("1.1", "Evidence")                  -- Evidence || Reasoning
        ,("1.2", "Cause_to_perceive")
        ,("1.3", "Protest")])

    , ( "convince"
      , [("1.1", "Suasion")])

    , ( "amount"
      , [("1.1", "Amounting_to")
        ,("1.2", "Being_in_category")])

    , ( "air"
      , [("1.2", "Publishing")])                -- Publishing

    , ( "transform"
      , [("1.1", "Cause_change")   -- Cause_change || Undergo_transformation
        ,("1.4", "Cause_change")])

    , ( "sponsor"
      , [("1.1", "Funding")])

    , ( "slide"
      , [("1.1", "Motion")
        ,("1.2", "Motion")
        ,("1.3", "Change_position_on_a_scale")
        ,("1.4", "Process_continue")])

    , ( "secure"
      , [("1.1", "Getting")])

    , ( "satisfy"
      , [("1.1", "Satisfying")])

    , ( "pose"
      , [("1.1", "Topic")
        ,("1.3", "Posing_as")])

    , ( "love"
      , [("1.1", "Experiencer_focus")
        ,("1.2", "Likelihood")])        -- (Likelihood)

    , ( "illustrate"
      , [("1.1", "Cause_to_perceive")
        ,("1.2", "Evidence")])

    , ( "fire"
      , [("1.1", "Use_firearm")
        ,("1.2", "Firing")
        ,("1.7", "none")])

    , ( "escape"
      , [("1.1", "Escaping")
        ,("1.2", "Avoiding")
        ,("1.4", "Remembering_information")
        ,("1.6", "none")])

    , ( "display"
      , [("1.1", "Cause_to_perceive")
        ,("1.2", "Cause_to_perceive")])

    , ( "discourage"
      , [("1.1", "Experiencer_obj")
        ,("1.2", "Attempt_suasion")])

    , ( "underlie"
      , [("1.1", "Causation")])

    , ( "suspect"
      , [("1.1", "Awareness")
        ,("1.2", "Suspicion")])

    , ( "squeeze"
      , [("1.1", "Manipulation")
        ,("1.2", "Cause_to_move_in_place")
        ,("1.3", "Cause_to_move_in_place")            -- (Cause_to_move_in_place)
        ,("1.5", "Inclusion")])

    , ( "specify"
      , [("1.1", "Statement")])

    , ( "slash"
      , [("1.2", "Cause_change_of_position_on_a_scale")])

    , ( "reverse"
      , [("1.1", "Change_direction")
        ,("1.2", "Deciding")                         -- (Decision)
        ,("1.3", "Cause_change")])

    , ( "qualify"
      , [("1.1", "Meet_specifications")])

    , ( "prevail"
      , [("1.1", "Dominate_situation")
        ,("1.2", "Existence")])

    , ( "postpone"
      , [("1.1", "Change_event_time")])

    , ( "influence"
      , [("1.1", "Objective_influence")])   -- Objective_influence || Subjective_influence

    , ( "hang"
      , [("1.1", "Placing")
        ,("1.4", "Existence")              -- (Existence)
        ,("1.5", "Reliance")
        ,("1.6", "Waiting")
        ,("1.7", "Personal_relationship")])  -- (Personal_relationship) || (Experiencer_focus)

    , ( "free"
      , [("1.1", "Breaking_out_captive")
        ,("1.2", "Removing")
        ,("1.3", "Capacity")])

    , ( "examine"
      , [("1.1", "Scrutiny")
        ,("1.2", "Inspecting")
        ,("1.3", "Examination")])

    , ( "date"
      , [("1.1", "Personal_relationship")
        ,("1.2", "Sign")
        ,("1.3", "Origin")])

    ]
