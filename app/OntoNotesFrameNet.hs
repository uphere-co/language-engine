module OntoNotesFrameNet where

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
        ,("1.3", "Conditional_occurence")
        ,("1.4", "Statement")
        ,("1.5", "Spelling_and_pronunciation")
        ,("1.6", "none")])
      
    , ( "have"
      , [("1.1", "Possession")
        ,("1.2", "(Have_associated) || (copula)")
        ,("1.3", "Being_employed")
        ,("1.4", "Undergo || Perception_experience")
        ,("1.5", "Personal_relationship")
        ,("1.6", "Causation")
        ,("1.7", "(Getting)")
        ,("1.8", "Arranging")
        ,("1.9", "Giving_birth")
        ,("1.10", "Sex")
        ,("1.11", "idioms")
        ,("1.12", "none")])
      
    , ( "make"
      , [("1.1", "lightverb")
        ,("1.2", "Creating < Manufacturing")
        ,("1.3", "Causation")
        ,("1.4", "Causation <- force.v")
        ,("1.6", "(Coming_to_be) || (Becoming)")
        ,("1.7", "Getting")
        ,("1.8", "Accomplishment")
        ,("1.9", "Self-motion")
        ,("1.15", "Conduct")
        ,("1.16", "idioms")])

    , ( "take"
      , [("1.1", "Intentionally_act")
        ,("1.2", "lightverb")
        ,("1.3", "Taking_time || Have_as_requirement")
        ,("1.4", "Bringing / Taking")
        ,("1.5", "Taking")
        ,("1.6", "(Coming_to_be)")
        ,("1.7", "Categorization || Communicate_categorization || Regard")
        ,("1.8", "Ride_vehicle")
        ,("1.9", "Being_in_control")
        ,("1.11", "Removing || Taking")
        ,("1.12", "(Soaking_up)")
        ,("1.14.1", "idioms")
        ,("1.15", "none")])
    
    , ( "get"
      , [("1.1", "Getting || Grasp")
        ,("1.2", "Medical_conditions")
        ,("1.3", "Becoming")
        ,("1.4", "Arriving")
        ,("1.11.1", "Becoming")
        ,("1.11.2", "Arriving")
        ,("1.11.3", "Activity_start")
        ,("1.11.5", "(Activity_continue) | (State_continue)")
        ,("1.11.6", "Arriving")
        ,("1.11.7", "Activity_finish")
        ,("1.11.8", "Becoming_aware")
        ,("1.11.13", "(Personal_relationship)")
        ,("1.11.17", "Recovery")
        ,("1.11.19", "Avoiding")
        ,("1.11.20", "(Assistence)")
        ,("1.11.23", "Getting")
        ,("1.11.26", "Experiencer_focus")
        ,("1.11.33", "Waking_up")
        ,("1.11.37", "idioms")
        ,("1.11.38", "none")
        ,("1.12", "none")])

    , ( "sell"
      , [("1.1", "Commerce_sell")
        ,("1.2", "Suasion")
        ,("1.3", "(Surrendering_possession)")
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
        ,("1.10", "(Path_shape)")
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
        ,("1.3", "Expend_resources")
        ,("1.4", "(Frequency)")])

    , ( "do"
      , [("1.1", "Intentionally_act")
        ,("1.2", "Thriving")
        ,("1.3", "Meet_specifications")
        ,("1.5", "(Successful_action)")
        ,("1.7", "Intentionally_affect")])

    , ( "come"
      , [("1.1", "Arriving")
        ,("1.2", "Event")
        ,("1.3", "Becoming")
        ,("1.4", "(copula)")
        ,("1.5", "Origin")
        ,("1.6", "(Event) || (Coming_to_be)")
        ,("1.7", "(Amounting_to)")
        ,("1.8", "Coming_up_with || Becoming_aware")
        ,("1.9", "Cause_to_perceive")
        ,("1.11", "idioms")])

    , ( "buy"
      , [("1.1", "Commerce_buy")
        ,("1.2", "Attempt_suasion")
        ,("1.3", "Coming_to_believe")
        ,("1.4", "Funding")
        ,("1.6", "Taking")])

    , ( "include"
      , [("1.1", "Categorization || Inclusion")
        ,("1.2", "Inclusion")])

    , ( "give"
      , [("1.1", "Giving")
        ,("1.2", "Offering")
        ,("1.3", "(Deny_or_grant_permission) || (Reward_and_punishments)")
        ,("1.4", "(Giving) || (Eventive_affecting)")
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
        ,("1.2", "(Attention)")
        ,("1.3", "(Deserving)")
        ,("1.4", "(Intentionally_act)")
        ,("1.5", "none")])
      
    , ( "see"
      , [("1.1", "Perception_experience")
        ,("1.2", "Grasp")
        ,("1.3", "Perception_experience")
        ,("1.4", "Verify")
        ,("1.5", "(Attention)")
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
        ,("1.8", "(Statement)")
        ,("1.9", "Request")
        ,("1.11", "Cause_to_end <- abort.v")])

    , ( "continue"
      , [("1.1", "Process_continue, Activity_ongoing")
        ,("1.2", "Statement, Activity_ongoing")])
      
    , ( "think"
      , [("1.1", "Opinion")
        ,("1.2", "Cogitation")
        ,("1.3", "Remembering_experience || Remembering_information")
        ,("1.4", "Experiencer_focus")
        ,("1.7", "none")])

    , ( "hold"
      , [("1.1", "Manipulation <- grasp.v")
        ,("1.2", "(Detaining) || (Inhibit_movement) || (Thwarting)")
        ,("1.3", "(Activity_ongoing)")
        ,("1.4", "(Statement <- maintain.v)")
        ,("2.5", "Possession || Being_in_control")
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
      , [("1.1", "Change_poisition_on_a_scale || Motion_directional")
        ,("1.2", "Coming_to_be")
        ,("1.3", "(Catastrophe)")
        ,("1.4", "(Event)")
        ,("1.12", "Being_operational")
        ,("1.14", "Make_agreement_on_action")])

    , ( "know"
      , [("1.1", "Awareness")
        ,("1.2", "(Verification)")
        ,("1.3", "(Awareness)")
        ,("1.4", "(Perception_experience)")
        ,("1.5", "(Differentiation) || (Affirm_or_deny)")
        ,("1.7", "none")])

    , ( "find"
      , [("1.1", "Locating || Becoming_aware")
        ,("1.2", "Resolve_problem")
        ,("1.3", "(Coming_to_believe)")
        ,("1.4", "(Assessing)")
        ,("1.5", "Verdict")])

    , ( "offer"
      , [("1.1", "Offering")
        ,("1.2", "Cause_to_perceive")
        ,("1.3", "Commitment <- volunteer.v")
        ,("1.4", "(Communication)")
        ,("1.5", "Publishing")])
      
    , ( "become"
      , [("1.1", "Becoming")])

    , ( "help"
      , [("1.1", "Assistance")
        ,("1.3", "Avoiding")])

    , ( "increase"
      , [("1.1", "Change_position_on_a_scale")])

    , ( "show"
      , [("1.1", "Cause_to_perceive || Reveal_secret || Evidence")
        ,("1.2", "Arriving")
        ,("1.3", "Publishing")
        ,("1.6", "Request")
        ,("1.7", "Intentionally_act")])

    , ( "provide"
      , [("1.1", "Supply")
        ,("1.2", "Supply")
        ,("1.3", "(Have_as_requirement)")])

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
        ,("1.3", "(Communication)")])

    , ( "remain"
      , [("1.1", "State_continue")
        ,("1.2", "Remainder")])

    , ( "put"
      , [("1.1", "Placing")
        ,("1.2", "lightverb")
        ,("1.3", "Statement")
        ,("1.4", "Using_resource")
        ,("1.5", "Estimating")
        ,("1.6", "(Causation)")
        ,("1.7.1", "(Statement)")
        ,("1.7.2", "Placing")
        ,("1.7.3", "Commitment")
        ,("1.7.4", "Using_resource")
        ,("1.7.11", "Judgment_communication")
        ,("1.7.12", "Recording")
        ,("1.7.15", "Change_event_time")
        ,("1.7.17", "Cause_to_end")
        ,("1.7.19", "Installing")
        ,("1.7.21", "(Manufacturing) || (Product_development)")
        ,("1.7.24", "Storing")
        ,("1.7.28", "Cause_to_be_included")
        ,("1.7.29", "Tolerating")
        ,("1.7.32", "none")])

    , ( "raise"
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2", "Cause_motion")
        ,("1.3", "Communication_manner")
        ,("1.4", "Cause_change_of_position_on_a_scale")
        ,("1.5", "Growing_food || (Personal_relationship)")
        ,("1.6", "Cause_to_perceive || Topic")
        ,("1.9", "Cause_to_end")])

    , ( "lead"
      , [("1.1", "Causation")
        ,("1.2", "Cotheme")
        ,("1.3", "(Path_shape)")
        ,("1.4", "Leadership")
        ,("1.5", "Cotheme")])

    , ( "look"
      , [("1.1", "Scrutiny || Perception_active")
        ,("1.2", "Give_impression")
        ,("1.4", "(Attention)")
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
        ,("1.3", "Operating a system")
        ,("1.4", "Motion")
        ,("1.6", "(Cause_to_perceive)")
        ,("1.7", "(Seeking_to_acheive)")
        ,("1.10", "(Commerce_scenario) || (Committing_crime) || (Legality)")
        ,("1.13.4", "Expend_resource")
        ,("1.13.5", "Meet_with || Confronting_problem")
        ,("1.13.17", "Change_position_on_a_scale")
        ,("1.13.21", "none")
        ,("1.14", "none")])

    , ( "keep"
      , [("1.1", "Retaining")
        ,("1.2", "Retaining")
        ,("1.3", "Retaining")
        ,("1.4", "Preventing_or_letting")
        ,("1.5", "(Compliance)")
        ,("1.7", "(Perception_active)")
        ,("1.8.12", "none")])

    , ( "believe"
      , [("1.1", "Certainty")
        ,("1.2", "Religious_belief")])

    , ( "follow"
      , [("1.1", "Cotheme")
        ,("1.2", "Relative_time")
        ,("1.3", "Compliance")
        ,("1.4", "Perception_active")
        ,("1.5", "(Evidence) || (Causation)")
        ,("1.6", "Process_continue <- continue.v")])

    , ( "seem"
      , [("1.1", "Give_impression")])

    , ( "consider"
      , [("1.1", "Categorization")
        ,("1.2", "Cogitation")])

    , ( "seek"
      , [("1.1", "Seeking")])

    , ( "receive"
      , [("1.1", "Receiving")
        ,("1.2", "(Adopt_selection)")])

    , ( "build"
      , [("1.1", "Building")
        ,("1.2", "(Coming_to_be)")
        ,("1.3", "(Creating)")
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
      , [("1.1", "                 (Reasoning? Evidence?)")
        ,("1.3", "Placing")])

    , ( "leave"
      , [("1.1", "Departing")
        ,("1.2", "")
        ,("1.3")
        ,("1.5", "Inclusion")])

    , ( "set"
      , [("1.1", "Placing")
        ,("1.2", "Motion_directional")
        ,("1.3", "Arranging")
        ,("1.4", "Arranging")
        ,("1.5", "Causing")
        ,("1.6", "Cause_to_start")
        ,("1.8", "Arranging")])

    , ( "move"
      , [("1.1", "Motion - Cause_motion")
        ,("1.2", "Intentionally_act")
        ,("1.3", "Experiencer_obj")
        ,("1.4", "Residence")
        ,("1.5", "(Commerce_sell)")
        ,("1.6", "(Social_connection)")
        ,("1.7", "(Experiencer_obj) || (Attack)")])

    , ( "turn"
      , [("1.1", "Change_direction")
        ,("1.2", "Becoming")
        ,("1.3", "Undergo_change")
        ,("1.4", "Agree_or_refuse_to_act")
        ,("1.5", "Cause_change")
        ,("1.6", "Turning_out")
        ,("1.7", "Becoming_visible")
        ,("1.8", "Manufacturing")
        ,("1.11", "Submitting_documents || (Giving (?))")
        ,("1.13", "Improvement_or_decline")
        ,("1.14", "Gathering_up")
        ,("1.15", "idioms")])

    , ( "start"
      , [("1.1", "Process_start")
        ,("1.2", "Activity_start")
        ,("1.3", "Cause_to_start")
        ,("1.4", "Activity_start - Cause_to_start")])

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
      , [("1.1", "Cause_change_of_position_on_a_scale")
        ,("1.2")
        ,("1.3")])

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
        ,("1.4", "")
        ,("1.5", "Causation")])

    , ( "open"
      , [("1.1", "Change_accesibility")
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
      , [("1.1", "                  (Expend_resource? time.)")
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
        ,("1.4", "Meet_specification")
        ,("1.5", "Undergoing")])

    , ( "win"
      , [("1.1", "Win_prize || Beat_opponent")
        ,("1.2", "Win_prize || Getting")
        ,("1.3", "Cause_change")])

    , ( "drop"
      , [("1.1", "Motion_directional || Cause_motion")
        ,("1.2", "Change_position_on_a_scale || Cause_to_end")
        ,("1.3", "Communication")
        ,("1.6", "Expend_resource")
        ,("1.8", "Forgoing")
        ,("1.13.4", "Surrendering || Abandonment")])

    , ( "operate"
      , [("1.1", "Being_in_operation")
        ,("1.2", "Operating_a_system")])

    , ( "feel"
      , [("1.1", "Feeling || Perception_active || Perception_experience")
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
        ,("1.2", "Spelling_and_pronunciation")
        ,("1.5", "Cause_change_of_position_on_a_scale")
        ,("1.6", "")])

    , ( "yield"
      , [("1.1", "Earnings_and_losses || Creating")
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
      , [("1.1", "                          (Activity?)")
        ,("1.2", "Competition")
        ,("1.3", "                          (Function, Being_relevant, Importance?)")
        ,("1.4", "   Performers_and_roles")
        ,("1.5", "   Performers_and_roles")
        ,("1.6", "   Wagering")
        ,("1.7", "                          (Prevarication? <- kid.v)")
        ,("1.12.6", "                       (Attention? <- ignore.v)")
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
        ,("1.2", "(Containing <- contain.v)")
        ,("1.3", "(Being_obligated)")
        ,("1.5", "Conduct")
        ,("1.6", "(Emotion_directed <- mad.a)")
        ,("1.8", "(Activity_ongoing)")])

    , ( "pass"
      , [("1.1", "Passing")
        ,("1.2", "Success_or_failure")
        ,("1.3", "Process_end")
        ,("1.4", "Giving")
        ,("1.7", "Path_shape")
        ,("1.8", "Giving")
        ,("1.11", "(Verification)")
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
      , [("1.1", "Success_or_failure   || Successful_action || Personal_success")
        ,("1.2", "Take_place_of")])

    , ( "charge"
      , [("1.1", "Commerce_collect")
        ,("1.2", "Imposing_obligation")
        ,("1.3", "Judgment_communication")])

    , ( "reflect"
      , [("1.1", "(Evidence <- show.v)")])

    , ( "return"
      , [("1.1", "Arriving || Rejuvenation || Resurrection")
        ,("1.2", "(Repayment <- pay back.v, Earnings_and_losses <- earn.v)")
        ,("1.3", "Response")
        ,("1.4", "Response")])

    , ( "cover"
      , [("1.1", "Distributed_position      (how to make causative?)")
        ,("1.2", "Topic || ( Sufficiency <- suffice.v)")
        ,("1.3", "Protecting")
        ,("1.4", "Hiding_objects || Prevarication")
        ,("1.8", "none")])

    , ( "expand"
      , [("1.1", "Expansion - Cause_expansion")])

    , ( "serve"
      , [("1.1", "Serving_in_capacity")
        ,("1.2", "Function")
        ,("1.3", "                    (Being_obligated)")
        ,("1.4", "                    (Giving)")
        ,("1.5", "Imprisonment")])

    , ( "complete"
      , [("1.1", "Activity_finish")])

    , ( "introduce"
      , [("1.1", "Make_acquaintance")
        ,("1.2", "(Process_start, Achieving_first, First_experience)")])

    , ( "stop"
      , [("1.1", "Activity_stop")
        ,("1.2", "Thwarting || Preventing_or_letting")
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
        ,("1.2", "Leadership || Relative_time")
        ,("1.6", "Thwarting")])

    , ( "push"
      , [("1.1", "Cause_motion")
        ,("1.2", "Seeking_to_achieve")
        ,("1.3", "(Attempt_suasion)")
        ,("1.6.1", "Cause_motion")
        ,("1.6.2", "(Ontogeny)")])

    , ( "purchase"
      , [("1.1", "Commerce_buy")])

    , ( "hit"
      , [("1.1", "Hit_target")
        ,("1.2", "Event || Impact")
        ,("1.3", "Arriving || Hit_target")])

    , ( "join"
      , [("1.1", "Cause_to_amalgamate || Attaching")
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
        ,("1.4", "(Institutionalization <- admit.v)")
        ,("1.5", "(Retaining <- retain.v)")])

    , ( "talk"
      , [("1.1", "Communication")
        ,("1.2", "Reveal_secret")
        ,("1.3", "Speak_on_topic")
        ,("1.9", "none")])

    , ( "predict"
      , [("1.1", "Predicting")])

    , ( "back"
      , [("1.2", "Taking_sides || Funding || Evidence")
        ,("1.4", "Duplication <- copy.v")
        ,("1.5", "(Cause_to_end <- abort.v, Withdraw_from_participation <- withdraw.v)")
        ,("1.6", "(Cause_to_end <- abort.v, Withdraw_from_participation <- withdraw.v)")
        ,("1.9", "none")])

    , ( "cost"
      , [("1.1", "Expensiveness")])

    , ( "invest"
      , [("1.1", "Funding || (Commmerce_buy)")
        ,("1.2", "(Have_associated <- have.v)")])

    , ( "break"
      , [("1.1", "Experience_bodily_harm - Cause_harm")
        ,("1.2", "Render_nonfunctional")
        ,("1.3", "(Process_stop <- stop.v)")
        ,("1.4", "Compliance")
        ,("1.5", "(Compliance <- obey.v)")
        ,("1.7", "Event")
        ,("1.16.1", "Breaking_apart")
        ,("1.16.2", "Render_nonfunctional")
        ,("1.16.3", "(Process_stop <- stop.v)")
        ,("1.16.4", "Process_start")
        ,("1.16.5", "(Placing <- insert.v)")
        ,("1.16.9", "(Event?, Statement?, Process_completed_state?)")
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
        ,("1.4", "(Cause_change_of_consistency <- set.v)")
        ,("1.5", "(Motion_directional <- drop.v)")])

    , ( "quote"
      , [("1.1", "(Statement)")
        ,("1.2", "Reference_text")
        ,("1.3", "Commerce_scenario")])

    , ( "stand"
      , [("1.1", "Posture - Change_posture")
        ,("1.2", "Being_located - Placing")
        ,("1.3", "(Give_impression <- seem.v, copula)")
        ,("1.4", "Tolerating")
        ,("1.6", "(Being_obligated)")
        ,("1.7.2", "Taking_sides")
        ,("1.7.4", "Representing")
        ,("1.7.9", "(Compliance <- disobey.v, Taking_sides)")
        ,("1.7.11", "(Cause_to_perceive)")
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
      , [("1.1", "(Intentionally_create <- create.v)")
        ,("1.2", "Coming_up_with")
        ,("1.3", "Purpose")])

    , ( "result"
      , [("1.1", "Causation")])

    , ( "oppose"
      , [("1.1", "Taking_sides")
        ,("1.2", "Competition")])

    , ( "vote"
      , [("1.1", "(Choosing? Political_actions?, Deciding?)")
        ,("1.4", "(Choosing? Political_actions?, Deciding?)")])

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
        ,("1.2", "Explainig_the_facts")])

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
        ,("1.2", "Explaining_the_facts (Justifying?)")])

    , ( "deny"
      , [("1.1", "Affirm_or_deny")
        ,("1.2", "Prevent_or_allow_possession")])

    , ( "avoid"
      , [("1.1", "Avoiding")])

    , ( "watch"
      , [("1.1", "Perception_active")
        ,("1.2", "Be_on_alert <- alert.v             * Attention")
        ,("1.3", "(Scrutiny <- monitor.v)")])

    , ( "replace"
      , [("1.1", "Replacing - Take_place_of")])

    , ( "assume"
      , [("1.1", "Awareness || Conditional_occurence")
        ,("1.2", "(Activity_start <- begin.v)")])

    , ( "reject"
      , [("1.1", "Respond_to_proposal")])

    , ( "kill"
      , [("1.1", "Killing")
        ,("1.3", "Erasing || (Process_stop?)")
        ,("1.4", "Thwarting")])

    , ( "ease"
      , [("1.1", "(Hindering?)")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.3", "Motion - Cause_motion")])

    , ( "drive"
      , [("1.1", "Operating_vehicle")
        ,("1.2", "Causation")
        ,("1.5", "Self-motion")])

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
        ,("1.2", "(Grasp <- understand.v)")
        ,("1.3", "(Grasp <- understand.v)")
        ,("1.4", "Reading_aloud")])

    , ( "like"
      , [("1.1", "Desiring <- want.v")
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
      , [("1.1", "Path_shape || Arriving || Transition_to_a_situation || Becoming_a_member")
        ,("1.2", "Cause_to_be_included <- add.v")])

    , ( "aim"
      , [("1.1", "Aiming")
        ,("1.2", "Purpose")])

    , ( "act"
      , [("1.1", "Performers_and_roles")
        ,("1.2", "Intentionally_act")])

    , ( "contend"
      , [("1.1", "Statement")
        ,("1.2", "(Competition?)")])

    , ( "prove"
      , [("1.1", "Reasoning || Evidence")
        ,("1.2", "Turning_out")
        ,("1.3", "(Deserving)")])

    , ( "elect"
      , [("1.1", "Change_of_leadership")
        ,("1.2", "Choosing")])

    , ( "determine"
      , [("1.1", "(Becoming_aware <- learn.v)")
        ,("1.2", "Deciding")
        ,("1.3", "Contingency")])

    , ( "urge"
      , [("1.1", "Request")
        ,("1.2", "Attempt_suasion")])

    , ( "focus"
      , [("1.1", "(Path_shape)")
        ,("1.2", "(Attention)")])

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
        ,("1.3", "(Emanating <- secrete.v, Emitting <- secrete.v)")])

    , ( "prepare"
      , [("1.1", "Activity_prepare")
        ,("1.2", "Activity_prepare")])

    , ( "worry"
      , [("1.1", "Emotion_active")])

    , ( "order"
      , [("1.1", "Request")
        ,("1.2", "Request_entity")])

    , ( "sit"
      , [("1.1", "Posture || Placing")
        ,("1.2", "Being_located")
        ,("1.3", "(Being_active <- inactive.a)")
        ,("1.4", "Participation")
        ,("1.5", "Being_awake")
        ,("1.8", "none")])

    , ( "encourage"
      , [("1.1", "Attempt_suasion")
        ,("1.2", "Subjective_influence <- inspire.v")])

    , ( "deliver"
      , [("1.1", "Creating")
        ,("1.4", "Intentionally_act")
        ,("1.5", "Delivery")
        ,("1.8", "none")])

    , ( "complain"
      , [("1.1", "Complaining")
        ,("1.2", "(Judgment_communication <- charge.v)")])

    , ( "understand"
      , [("1.1", "Grasp")
        ,("1.2", "Awareness")
        ,("1.3", "Reasoning")])

    , ( "pull"
      , [("1.1", "Cause_motion")
        ,("1.2", "Cause_motion")
        ,("1.3", "Removing || Activity_stop")
        ,("1.4", "Intentionally_act")
        ,("1.7", "(Surviving, Recovery)")
        ,("1.10", "idioms")])

    , ( "place"
      , [("1.1", "Placing")
        ,("1.4", "Arranging")
        ,("1.7", "(Funding, Commercial_buy <- invest.v)")])

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
        ,("1.7", "Coming_up_with <- formulate.v")
        ,("1.11", "idioms")])

    , ( "warn"
      , [("1.1", "Warning")])

    , ( "suffer"
      , [("1.1", "Catastrophe || Eventive_affecting")
        ,("1.2", "Imporovement_or_decline")])

    , ( "publish"
      , [("1.1", "Publishing")])

    , ( "fight"
      , [("1.1", "Hostile_encounter")
        ,("1.2", "Seeking_to_achieve")
        ,("1.3", "Seeking_to_achieve")])

    , ( "earn"
      , [("1.1", "Earnings_and_losses")
        ,("1.2", "(Deserving)")])

    , ( "apply"
      , [("1.1", "Using")
        ,("1.2", "Being_relevant")
        ,("1.3", "(Request)")
        ,("1.4", "Using")])

    , ( "rule"
      , [("1.1", "Being_in_control  * Dominate_situation, Leadership")
        ,("1.2", "Verdict")
        ,("1.3", "Inclusion <- exclude.v")])

    , ( "form"
      , [("1.1", "Coming_to_be - Creating")
        ,("1.2", "Coming_to_be")
        ,("1.3", "Intentionally_create")
        ,("1.4", "Coming_to_be")
        ,("1.5", "none")])

    , ( "fill"
      , [("1.1", "Filling")
        ,("1.2", "Employing")
        ,("1.3", "Meet_specifications")
        ,("1.4", "(Activity_finish <- complete.v)")
        ,("1.5", "(Explaining_the_facts <- explain.v )")])

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
        ,("1.7.1", "(Placing)")
        ,("1.7.2", "Getting")
        ,("1.7.3", "Becoming_aware")
        ,("1.7.4", "Changing_position_on_a_scale - Cause_cahnge_of_position_on_a_scale")
        ,("1.7.6", "Choosing")
        ,("1.7.8", "Hit_or_miss")
        ,("1.7.10", "(Personal_relationship <- date.v)")])

    , ( "describe"
      , [("1.1", "Communicate_categorization || (Statement || Explaining_the_fact)")
        ,("1.4", "Statement")])

    , ( "save"
      , [("1.1", "Rescuing")
        ,("1.2", "Storing <- keep.v")
        ,("1.3", "(Frugality <- waste.v)")])

    , ( "relate"
      , [("1.1", "Relating_concepts")
        ,("1.2", "Relating_concepts")])

    , ( "refuse"
      , [("1.1", "Agree_or_refuse_to_act || Respond_to_request")
        ,("1.2", "Deny_or_grant_permission")])

    , ( "hurt"
      , [("1.2", "Eventive_affecting || Intentionally_affect")
        ,("1.3", "Experiencer_obj")
        ,("1.4", "Desiring")])

    , ( "hire"
      , [("1.1", "Hiring")])

    , ( "die"
      , [("1.1", "Ceasing_to_be")
        ,("1.2", "Change_position_on_a_scale")
        ,("1.4", "(Being_operational <- broken.a)")])

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
        ,("1.2", "(Forming_relationships) || (Thriving)")
        ,("1.3", "Topic")
        ,("1.4", "Dispersal")])

    , ( "account"
      , [("1.1", "Justifying")
        ,("1.2", "(Amounting_to)")])

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
      , [("1.1", "Containing || Inclusion  || (Membership)")
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
        ,("1.2", "(Abandonment) || (Removing) || (Process_stop) || (Agree_or_refuse_to_act)")
        ,("1.5", "Experiencer_obj")
        ,("1.6", "(Arranging <- set up.v)")])

    , ( "range"
      , [("1.1", "Delimitation_of_diversity")])

    , ( "declare"
      , [("1.1", "Statement")])

    , ( "admit"
      , [("1.1", "Affirm_or_deny")
        ,("1.2", "Institutionalization")])

    , ( "catch"
      , [("1.1", "Taking_captive || Experience_obj")
        ,("1.2", "Taking_captive || Perception_active")
        ,("1.4", "(Accomplishment)")
        ,("1.5", "Come_down_with || Catching_fire")
        ,("1.6", "Attending")
        ,("1.7", "Process_start")
        ,("1.8", "Getting")])

    , ( "attempt"
      , [("1.1", "Attempt")])

    , ( "adjust"
      , [("1.1", "Adjusting")
        ,("1.2", "(Adopting_selection)")])

    , ( "speak"
      , [("1.1", "Communication")
        ,("1.2", "Speak_on_topic")
        ,("1.4", "Leadership")
        ,("1.5", "(Communication)")])

    , ( "list"
      , [("1.1", "(Categorization)")
        ,("1.2", "Change_posture")
        ,("1.3", "none")])

    , ( "extend"
      , [("1.2", "Change_event_duration || (Change_position_on_a_scale)")
        ,("1.3", "Offering")])

    , ( "respond"
      , [("1.1", "Response")
        ,("1.2", "Communication_response")])

    , ( "remove"
      , [("1.1", "Removing")])

    , ( "recover"
      , [("1.1", "Recovery")
        ,("1.2", "Rejuvenation")
        ,("1.3", "(Reparation) || (Rejuvenation)")])

    , ( "realize"
      , [("1.1", "Coming_to_believe")
        ,("1.2", "(Accomplishment)")
        ,("1.3", "Earnings_or_losses")])

    , ( "fly"
      , [("1.1", "Motion - Cause_motion")
        ,("1.5", "Success_or_failure")])

    , ( "view"
      , [("1.1", "Categorization")
        ,("1.2", "Perception_active")])

    , ( "study"
      , [("1.1", "Scrutiny")
        ,("1.2", "Studying")])

    , ( "recall"
      , [("1.1", "Remembering_experience || Remembering_information")
        ,("1.2", "(Process_stop?)")])

    , ( "discover"
      , [("1.1", "Becoming_aware")])

    , ( "compete"
      , [("1.1", "Competition")])

    , ( "guarantee"
      , [("1.1", "(Commitment <- promise.v)")
        ,("1.2", "Evidence")])

    , ( "generate"
      , [("1.1", "Cause_to_start")
        ,("1.2", "(Building) || (Creating)")])

    , ( "refer"
      , [("1.2", "(Topic) || (Mention)")
        ,("1.3", "(Telling)")])

    , ( "pursue"
      , [("1.1", "Seeking_to_acheive || Seeking")
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
        [("1.1", "Quitting")
        ,("1.3", "Process_stop")
        ,("1.6", "(Remove?)")])

    , ( "permit"
      , [("1.1", "Preventing_or_letting")])

    , ( "emerge"
      , [("1.1", "Departing")
        ,("1.2", "Coming_to_be")])

    , ( "damage"
      , [("1.1", "Damaging")])

    , ( "concern"
      , [("1.1", "Being_relevant")
        ,("1.2", "(Experiencer_obj <- worry.v) || (Emotion_active <- worry.v)")])

    , ( "violate"
      , [("1.1", "Compliance")
        ,("1.2", "Damaging")])

    , ( "resume"
      , [("1.1", "Activity_resume || Cause_to_resume")
        ,("1.2", "Process_resume")])

    , ( "match"
      , [("1.1", "Evaluative_comparison")
        ,("1.2", "Funding")
        ,("1.3", "(Choosing)")
        ,("1.4", "Competition")])

    , ( "value"
      , [("1.1", "Assessing")
        ,("1.2", "Judgment")])

    , ( "suspend"
      , [("1.2", "(Activity_pause)")
        ,("1.3", "Activity_pause")])

    , ( "stem"
      , [("1.1", "Origin <- originate.v")
        ,("1.3", "Hindering")
        ,("1.5", "(Resolving_problem <- deal.v)")])

    , ( "promote"
      , [("1.1", "(Cause_to_make_progress) | (Cause_to_expand)")
        ,("1.2", "(Cause_change_of_position_on_a_scale)")])

    , ( "market"
      , [("1.1", "(Commerce_sell)")])

    , ( "prompt"
      , [("1.1", "Cause_to_start")])

    , ( "ignore"
      , [("1.1", "Regard || Attention")])

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
      , [("1.1", "Judgement_communication")
        ,("1.2", "Judgement")])

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
      , [("1.1", "(copula)")
        ,("1.2", "Giving_birth")
        ,("1.3", "Creating")
        ,("1.5", "Tolerating")])

    , ( "strike"
      , [("1.1", "Attack || Impact")
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
        ,("1.3", "(Possesion)")
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
      , [("1.1", "Cause_to_perceive || (Publishing)")
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
        ,("1.5", "(Request <- petition.n)")
        ,("1.8", "Cause_change")
        ,("1.10", "none")])

    , ( "perform"
      , [("1.1", "Intentionally_act")
        ,("1.2", "Performers_and_roles")
        ,("1.3", "none")])

    , ( "favor"
      , [("1.1", "Partiality")])

    , ( "convert"
      , [("1.1", "Undergo_change | Undergo_transformation - Cause_change   | Exchange_currency")
        ,("1.2", "Cause_change")])

    , ( "borrow"
      , [("1.1", "Borrowing")
        ,("1.2", "(Adopt_selection)")])

    , ( "restrict"
      , [("1.1", "Limiting")])

    , ( "restore"
      , [("1.1", "(Transition_to_state - Cause_change) || (Rejuvenation)")])

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
   ]
