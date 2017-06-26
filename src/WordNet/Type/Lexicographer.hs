{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module WordNet.Type.Lexicographer where

import           Control.Lens
import           Data.Text
-- import qualified Data.Text as T
--
import           NLP.Type.WordNet


data LexicographerFile = AdjAll            -- 00
                       | AdjPert           -- 01
                       | AdvAll            -- 02
                       | NounTops          -- 03
                       | NounAct           -- 04
                       | NounAnimal        -- 05
                       | NounArtifact      -- 06
                       | NounAttribute     -- 07
                       | NounBody          -- 08
                       | NounCognition     -- 09
                       | NounCommunication -- 10
                       | NounEvent         -- 11
                       | NounFeeling       -- 12
                       | NounFood          -- 13
                       | NounGroup         -- 14
                       | NounLocation      -- 15
                       | NounMotive        -- 16
                       | NounObject        -- 17
                       | NounPerson        -- 18
                       | NounPhenomenon    -- 19
                       | NounPlant         -- 20
                       | NounPossession    -- 21
                       | NounProcess       -- 22
                       | NounQuantity      -- 23
                       | NounRelation      -- 24
                       | NounShape         -- 25
                       | NounState         -- 26
                       | NounSubstance     -- 27
                       | NounTime          -- 28
                       | VerbBody          -- 29
                       | VerbChange        -- 30
                       | VerbCognition     -- 31
                       | VerbCommunication -- 32
                       | VerbCompetition   -- 33
                       | VerbConsumption   -- 34
                       | VerbContact       -- 35
                       | VerbCreation      -- 36
                       | VerbEmotion       -- 37
                       | VerbMotion        -- 38
                       | VerbPerception    -- 39
                       | VerbPossession    -- 40
                       | VerbSocial        -- 41
                       | VerbStative       -- 42
                       | VerbWeather       -- 43
                       | AdjPpl            -- 44
                       deriving (Show,Eq,Ord,Enum)


lexicographerFileToSSType :: LexicographerFile -> SSType
lexicographerFileToSSType AdjAll            = Adjective
lexicographerFileToSSType AdjPert           = Adjective
lexicographerFileToSSType AdvAll            = Adverb
lexicographerFileToSSType NounTops          = Noun
lexicographerFileToSSType NounAct           = Noun
lexicographerFileToSSType NounAnimal        = Noun
lexicographerFileToSSType NounArtifact      = Noun
lexicographerFileToSSType NounAttribute     = Noun
lexicographerFileToSSType NounBody          = Noun
lexicographerFileToSSType NounCognition     = Noun
lexicographerFileToSSType NounCommunication = Noun
lexicographerFileToSSType NounEvent         = Noun
lexicographerFileToSSType NounFeeling       = Noun
lexicographerFileToSSType NounFood          = Noun
lexicographerFileToSSType NounGroup         = Noun
lexicographerFileToSSType NounLocation      = Noun
lexicographerFileToSSType NounMotive        = Noun
lexicographerFileToSSType NounObject        = Noun
lexicographerFileToSSType NounPerson        = Noun
lexicographerFileToSSType NounPhenomenon    = Noun
lexicographerFileToSSType NounPlant         = Noun
lexicographerFileToSSType NounPossession    = Noun
lexicographerFileToSSType NounProcess       = Noun
lexicographerFileToSSType NounQuantity      = Noun
lexicographerFileToSSType NounRelation      = Noun
lexicographerFileToSSType NounShape         = Noun
lexicographerFileToSSType NounState         = Noun
lexicographerFileToSSType NounSubstance     = Noun
lexicographerFileToSSType NounTime          = Noun
lexicographerFileToSSType VerbBody          = Verb
lexicographerFileToSSType VerbChange        = Verb
lexicographerFileToSSType VerbCognition     = Verb
lexicographerFileToSSType VerbCommunication = Verb
lexicographerFileToSSType VerbCompetition   = Verb
lexicographerFileToSSType VerbConsumption   = Verb
lexicographerFileToSSType VerbContact       = Verb
lexicographerFileToSSType VerbCreation      = Verb
lexicographerFileToSSType VerbEmotion       = Verb
lexicographerFileToSSType VerbMotion        = Verb
lexicographerFileToSSType VerbPerception    = Verb
lexicographerFileToSSType VerbPossession    = Verb
lexicographerFileToSSType VerbSocial        = Verb
lexicographerFileToSSType VerbStative       = Verb
lexicographerFileToSSType VerbWeather       = Verb
lexicographerFileToSSType AdjPpl            = Adjective


lexicographerFileTable :: [(Text,Either Text LexicographerFile)]
lexicographerFileTable = [ ("adj.all"           , Right AdjAll           )
                         , ("adj.pert"          , Right AdjPert          )
                         , ("adv.all"           , Right AdvAll           )
                         , ("noun.act"          , Right NounAct          )
                         , ("noun.animal"       , Right NounAnimal       )
                         , ("noun.artifact"     , Right NounArtifact     )
                         , ("noun.attribute"    , Right NounAttribute    )
                         , ("noun.body"         , Right NounBody         )
                         , ("noun.cognition"    , Right NounCognition    )
                         , ("noun.communication", Right NounCommunication)
                         , ("noun.event"        , Right NounEvent        )
                         , ("noun.feeling"      , Right NounFeeling      )
                         , ("noun.food"         , Right NounFood         )
                         , ("noun.group"        , Right NounGroup        )
                         , ("noun.location"     , Right NounLocation     )
                         , ("noun.motive"       , Right NounMotive       )
                         , ("noun.object"       , Right NounObject       )
                         , ("noun.person"       , Right NounPerson       )
                         , ("noun.phenomenon"   , Right NounPhenomenon   )
                         , ("noun.plant"        , Right NounPlant        )
                         , ("noun.possession"   , Right NounPossession   )
                         , ("noun.process"      , Right NounProcess      )
                         , ("noun.quantity"     , Right NounQuantity     )
                         , ("noun.relation"     , Right NounRelation     )
                         , ("noun.shape"        , Right NounShape        )
                         , ("noun.state"        , Right NounState        )
                         , ("noun.substance"    , Right NounSubstance    )
                         , ("noun.time"         , Right NounTime         )
                         , ("verb.body"         , Right VerbBody         )
                         , ("verb.change"       , Right VerbChange       )
                         , ("verb.cognition"    , Right VerbCognition    )
                         , ("verb.communication", Right VerbCommunication)
                         , ("verb.competition"  , Right VerbCompetition  )
                         , ("verb.consumption"  , Right VerbConsumption  )
                         , ("verb.contact"      , Right VerbContact      )
                         , ("verb.creation"     , Right VerbCreation     )
                         , ("verb.emotion"      , Right VerbEmotion      )
                         , ("verb.motion"       , Right VerbMotion       )
                         , ("verb.perception"   , Right VerbPerception   )
                         , ("verb.possession"   , Right VerbPossession   )
                         , ("verb.social"       , Right VerbSocial       )
                         , ("verb.stative"      , Right VerbStative      )
                         , ("verb.weather"      , Right VerbWeather      )
                         , ("adj.ppl"           , Right AdjPpl           )
                         -- meta files
                         , ("noun.Tops"         , Left "noun.Tops")
                         -- , ("verb.Framestext"   , Left "verb.Framestext")
                         ]


data PointerSymbol = Antonym                    --   "!"
                   | Hypernym                   --   "@"
                   | Instance_Hypernym          --   "@i"
                   | Hyponym                    --   "~"
                   | Instance_Hyponym           --   "~i"
                   | Member_Holohym             --   "#m"
                   | Substance_Holonym          --   "#s"
                   | Part_Holonym               --   "#p"
                   | Member_Meronym             --   "%m"
                   | Substance_Meronym          --   "%s"
                   | Part_Meronym               --   "%p"
                   | Attribute                  --   "="
                   | DerivationallyRelatedForm  --   "+"

                   | Entailment                 --   "*"
                   | Cause                      --   ">"
                   | AlsoSee                    --   "^"
                   | VerbGroup                  --   "$"
                   | SimilarTo                  --   "&"
                   | ParticipleOfVerb           --   "<"
                   | BackSlash                  --   "\"  adj: (pertains to noun), adv: (derived from adjective)
                   | DomainOfSynset_TOPIC       --   ";c"
                   | MemberOfThisDomain_TOPIC   --   "-c"
                   | DomainOfSynset_REGION      --   ";r"
                   | MemberOfThisDomain_REGION  --   "-r"
                   | DomainOfSynset_USAGE       --   ";u"
                   | MemberOfThisDomain_USAGE   --   "-u"
                   deriving (Show,Eq,Ord)


data Marker = Marker_P  -- ^ predicate position
            | Marker_A  -- ^ prenominal (attributive) position
            | Marker_IP -- ^ immediately postnominal position
            deriving Show


data SSWord = SSWord { _ssw_word   :: [Text]
                     , _ssw_marker :: Maybe Marker
                     , _ssw_lexid  :: Maybe Int }
            deriving Show


data SSPointer
  = SSPointer { _ssp_lex_filename   :: Maybe (Either Text LexicographerFile)
              , _ssp_word           :: [Text]
              , _ssp_lexid          :: Maybe Int
              , _ssp_satellite      :: Maybe ([Text],Maybe Int)
              , _ssp_pointer_symbol :: PointerSymbol
              }
  deriving Show


data Synset
  = Synset { _ssn_words_or_wordpointers    :: [Either SSWord (SSWord,[SSPointer],[Int])]
           , _ssn_pointers :: [SSPointer]
           , _ssn_frames   :: [Int]
           , _ssn_gloss    :: Text }
  deriving Show

makeLenses ''Synset

