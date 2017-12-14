{-# LANGUAGE OverloadedStrings #-}

module SRL.Analyze.Match.Preposition where

import           Control.Monad (join)
import           Data.List     (lookup)
import           Data.Maybe    (maybeToList)
import           Data.Text     (Text)
--
import           Lexicon.Type  (FNFrame,FNFrameElement)



ppRelFrame :: Text -> Maybe (FNFrame,FNFrameElement,FNFrameElement)
ppRelFrame p = lookup p [ ("about"     , ("Topic"                        , "Text"     , "Topic"))
                        , ("above"     , ("Directional_locative_relation", "Figure"   , "Ground"))
                        , ("across"    , ("Distributed_position"         , "Theme"    , "Location"))
                        , ("after"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("against"   , ("Taking_sides"                 , "Cognizer" , "Issue"))
                        , ("along"     , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- alongside
                        , ("amid"      , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("amidst"    , ("Contrary_circumstances"       , "Event"    , "Adversity"))
                        , ("among"     , ("Be_subset_of"                 , "Part"     , "Total"))
                        , ("around"    , ("Distributed_position"         , "Theme"    , "Location"))
                        , ("as"        , ("Performers_and_roles"         , "Performer", "Role"))
                        , ("astride"   , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("at"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("atop"      , ("Spatial_contact"              , "Figure"   , "Ground"))
                        -- ontop
                        -- bar
                        , ("before"    , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("behind"    , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("below"     , ("Directional_locative_relation", "Figure"   , "Ground"))
                        , ("beneath"   , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("beside"    , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("besides"   , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("between"   , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("beyond"    , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- but
                        , ("by"        , ("Means"                        , "Purpose"  , "Means" ))
                        -- circa
                        -- come
                        , ("despite"   , ("Concessive"                   , "Main_assertion", "Conceded_state_of_affairs"))
                        , ("down"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("during"    , ("Temporal_collocation"         , "Trajector_event", "Landmark_event"))
                        -- except
                        , ("for"       , ("Purpose"                      , "Means"    , "Goal"))
                        , ("from"      , ("Origin"                       , "Entity"   , "Origin"))
                        , ("in"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("inside"    , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("into"      , ("Goal"                         , "Trajector", "Landmark"))
                        -- less
                        , ("like"      , ("Similarity"                   , "Entity_1" , "Entity_2"))
                        -- minus
                        , ("near"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- notwithstanding
                        , ("of"        , ("Partitive"                    , "Subset"   , "Group"))
                        , ("off"       , ("Spatial_contact"              , "Figure"   , "Ground"))
                        , ("on"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- onto
                        -- opposite
                        , ("out"       , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("outside"   , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("over"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("past"      , ("Locative_relation"            , "Figure"   , "Ground"))
                        -- per
                        -- save
                        -- short
                        , ("since"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        -- than
                        , ("through"   , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("througout" , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("to"        , ("Goal"                         , "Trajector", "Landmark"))
                        , ("toward"    , ("Goal"                         , "Trajector", "Landmark"))
                        , ("towards"   , ("Goal"                         , "Trajector", "Landmark"))
                        , ("under"     , ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("underneath", ("Non-gradable_proximity"       , "Figure"   , "Ground"))
                        , ("unlike"    , ("Similarity"                   , "Entity_1" , "Entity_2"))
                        , ("until"     , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("till"      , ("Time_vector"                  , "Event"    , "Landmark_event"))
                        , ("up"        , ("Locative_relation"            , "Figure"   , "Ground"))
                        , ("upon"      , ("Spatial_contact"              , "Figure"   , "Ground"))
                        -- upside
                        -- versus
                        -- via
                        , ("with"      , ("Accompaniment"                , "Participant","Co-participant"))
                        , ("within"    , ("Interior_profile_relation"    , "Figure"   , "Ground"))
                        , ("without"   , ("Negation"                     , "Factual_situation", "Negated_proposition"))
                        -- worth
                        ]



ppExtraRoles :: Text -> [FNFrameElement]
ppExtraRoles p = join (maybeToList (lookup p ppExtraRoleMap))


ppExtraRoleMap :: [(Text,[FNFrameElement])]
ppExtraRoleMap = [ ("about"     , ["Topic"])
                 , ("above"     , ["Location","Place"])
                 , ("across"    , ["Path"])
                 , ("after"     , ["Time"])
                 , ("against"   , ["Issue"])
                 , ("along"     , ["Path"])
                 -- alongside
                 , ("amid"      , ["Duration"])
                 , ("amidst"    , ["Duration"])
                 , ("among"     , ["Total","Possibilities"])
                 , ("around"    , ["Area","Location"])
                 , ("as"        , ["Role"])
                 , ("astride"   , ["Location","Place"])
                 , ("at"        , ["Location","Place","Time"])
                 , ("atop"      , ["Location"])
                 -- ontop
                 -- bar
                 , ("before"    , ["Time"])
                 , ("behind"    , ["Location","Place"])
                 , ("below"     , ["Location","Place"])
                 , ("beneath"   , ["Location","Place"])
                 , ("beside"    , ["Location","Place"])
                 , ("besides"   , ["Location","Place"])
                 , ("between"   , ["Area","Location","Place"])
                 , ("beyond"    , ["Standard_item","Domain"])
                 -- but
                 , ("by"        , ["Means","Location","Place"])
                 -- circa
                 -- come
                 , ("despite"   , ["Event_description"])
                 , ("down"      , ["Direction"])
                 , ("during"    , ["Duration"])
                 -- except
                 , ("for"       , ["Purpose","Duration","Goal","Demands"])
                 , ("from"      , ["Origin","Support"])
                 , ("in"        , ["Location","Place","Time"])
                 , ("inside"    , ["Area","Location","Ground"])
                 , ("into"      , ["Goal","Effect","Final_category"])
                 -- less
                 , ("like"      , ["Manner"])
                 -- minus
                 , ("near"      , ["Location","Place"])
                 -- notwithstanding
                 , ("of"        , ["Phenomenon"])
                 , ("off"       , ["Location","Place"])
                 , ("on"        , ["Location","Place","Time"])
                 -- onto
                 -- opposite
                 , ("out"       , ["Direction"])
                 , ("outside"   , ["Area","Ground"])
                 , ("over"      , ["Duration","Location","Place","Area","Ground"])
                 , ("past"      , ["Path","Area","Ground","Ground"])
                 -- per
                 -- save
                 -- short
                 , ("since"     , ["Time"])
                 -- than
                 , ("through"   , ["Path","Duration","Ground"])
                 , ("througout" , ["Path","Duration","Ground"])
                 , ("to"        , ["Goal","Effect","Direction"])
                 , ("toward"    , ["Goal","Direction"])
                 , ("towards"   , ["Goal","Direction"])
                 , ("under"     , ["Location","Place","Ground","Area"])
                 , ("underneath", ["Location","Place"])
                 , ("unlike"    , ["Manner"])
                 , ("until"     , ["Time"])
                 , ("till"      , ["Time"])
                 , ("up"        , ["Direction"])
                 , ("upon"      , ["Location","Place"])
                 -- upside
                 -- versus
                 -- via
                 , ("with"      , ["Co-participant"])
                 , ("within"    , ["Location","Place","Ground","Area"])
                 , ("without"   , ["Co-participant"])
                 -- worth
                 ]
