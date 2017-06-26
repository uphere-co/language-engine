{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HM
import           Data.IntMap                (IntMap)
import qualified Data.IntMap         as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Environment
import           System.FilePath
--
import           NLP.Type.WordNet
-- import           WordNet.API.Query
import           WordNet.Query
import           WordNet.Parser.Sense
import           WordNet.Parser.Lexicographer
import           WordNet.Type
import           WordNet.Type.Lexicographer


testdata_noun
  = [ "{ lamivudine, 3TC, nucleoside_reverse_transcriptase_inhibitor,@ (a nucleoside reverse transcriptase inhibitor that is very effective in combination with zidovudine in treating AIDS and HIV) }\n"
    , "{ one-hitter, 1\"-hitter, baseball_game,@ (a game in which a pitcher allows the opposing team only one hit) }\n"
    , "{ radiocarbon_dating, carbon_dating, carbon-14_dating, dating,@ (a chemical analysis used to determine the age of organic materials based on their content of the radioisotope carbon 14; believed to be reliable up to 40,000 years) }\n"
    ]

testdata_verb
  = [ "{ [ breathe, noun.artifact:breather,+ noun.act:breathing,+ breathe_out,^ breathe_in,^ ] take_a_breath, [ respire, adj.pert:respiratory,+ noun.act:respiration1,+ noun.artifact:respirator,+ ] suspire3, inhale,* exhale,* frames: 2,8 (draw air into, and expel out of, the lungs; \"I can breathe better when the air is clean\"; \"The patient is respiring\") }\n"
    , "{ [ de-energize, energize,! ] [ de-energise, energise,! ]verb.change:weaken1,@ frames: 10 (deprive of energy) }\n"
    , "{ [ stretch1, noun.act:stretch,+ noun.act:stretching,+ frames: 2 ] [ extend, noun.act:extension1,+ noun.body:extensor,+ ] tense,@ frames: 8 (extend one's limbs or muscles, or the entire body; \"Stretch your legs!\"; \"Extend your right arm above your head\") }\n"
    , "{ [ reduce, noun.process:reducing1,+ gain,! ] [melt_off, frames: 8 ] slim, slenderize, thin, slim_down, verb.change:change_state,@ frames: 2 (take off weight) }\n"
    , "{ get_down, [ begin, noun.person:beginner,+ noun.act:beginning,+ end1,! ] get12, start_out, [ start, noun.act:start,+ noun.time:start,+ noun.event:start,+ noun.person:starter1,+ ] set_about, set_out, [ commence, noun.act:commencement,+ ] frames: 1,2, 28,33,8 (take the first step or steps in carrying out an action; \"We began working at dawn\"; \"Who will start?\"; \"Get working as soon as the sun rises!\"; \"The first tourists began to arrive in Cambodia\"; \"He began early in the day\"; \"Let's get down to work now\") }\n"
    , "{ [ induce, adj.all:causative^inducive,+ noun.motive:inducement,+ noun.act:induction3,+ noun.person:inducer,+ noun.act:inducing,+ frames: 24] [ stimulate1, noun.cognition:stimulation,+ noun.act:stimulation,+ noun.cognition:stimulus,+ frames: 24] [ cause, adj.all:causative,+ noun.act:causation,+ noun.event:cause,+ noun.communication:cause,+ noun.Tops:cause,+ frames: 24] have, [get,frames: 24] make, frames: 25 (cause to do; cause to act in a specified manner; \"The ads induced me to buy a VCR\"; \"My children finally got me to buy a computer\"; \"My wife made me buy a new sofa\") }\n"
    , "{ [ attract, adj.all:attractive1,+ noun.communication:attraction,+ noun.cognition:attraction,+ noun.attribute:attraction,+ noun.cognition:attractor,+ repel,!] [ appeal, noun.attribute:appeal,+ ] frames: 4 please,* frames: 10,9 (be attractive to; \"The idea of a vacation appeals to me\"; \"The beautiful garden attracted many people\") }\n"
    , "{ body-surf,glide,@ frames: 2 (ride the crest of a wave without a surfboard)}\n"
    ]

testdata_adverb
  = [ "{ [ unbearably, adj.all:unbearable,+ adj.all:unbearable,\\ ] (to an unbearable degree; \"it was unbearably hot in the room\") }\n"
    ]

testdata_adjective
  = [ "{ [ ridged, verb.change:ridge,< ] [ carinate, noun.animal:carinate,+ ] carinated, keeled, (having a ridge or shaped like a ridge or suggesting the keel of a ship; \"a carinate sepal\") }\n"
    ]

testdata_adj_cluster
  = [ "[{ [ ACIDIC, ALKALINE,! AMPHOTERIC,!] noun.cognition:chemistry,;c (being or containing an acid; of a solution having an excess of hydrogen atoms (having a pH of less than 7)) }\n{ [ acid, noun.substance:acid,+ noun.attribute:acidity2,+ ] noun.cognition:chemistry,;c (having the characteristics of an acid; \"an acid reaction\") }\n{ acid-forming, (yielding an acid in aqueous solution) }\n----\n{ [ ALKALINE, noun.attribute:alkalinity,+ AMPHOTERIC,! ACIDIC,!] [ alkalic, noun.substance:alkali1,+ ] noun.cognition:chemistry,;c (relating to or containing an alkali; having a pH greater than 7; \"alkaline soils derived from chalk or limestone\") }\n{ alkalescent, alcalescent, (tending to become alkaline; slightly alkaline) }\n{ basic, noun.cognition:chemistry,;c (of or denoting or of the nature of or containing a base) }\n{ base-forming, (yielding a base in aqueous solution) }\n{ saltlike, (resembling a compound formed by replacing hydrogen in an acid by a metal) }\n----\n{ [ AMPHOTERIC, ACIDIC,! ALKALINE,!] amphiprotic, noun.cognition:chemistry,;c (having characteristics of both an acid and a base and capable of reacting as either) }]\n"
    , "[{ [ ALIVE1(p), noun.attribute:aliveness,+ DEAD1,!] [ live, noun.attribute:liveness,+ ] ANIMATE1,^ noun.state:life,= noun.attribute:vitality,= (possessing life; \"the happiest person alive\"; \"the nerve is alive\"; \"doctors are working hard to keep him alive\"; \"burned alive\"; \"a live canary\") }\n\
      \{ liveborn, ((of newborn infant) showing signs of life after birth; not stillborn; \"a liveborn baby\") }\n\
      \{ [ viable, noun.attribute:viability,+ ] (capable of life or normal growth and development; \"viable seeds\"; \"a viable fetus\") }\n\
      \{ [ vital, verb.change:vitalize1,+ noun.attribute:vitalness,+ noun.attribute:vitality,+ ] (manifesting or characteristic of life; \"a vital, living organism\"; \"vital signs\") }\n\
      \----\n\
      \{ [ DEAD1, noun.attribute:deadness,+ ALIVE1,!] noun.state:life,= noun.attribute:vitality,= (no longer having or seeming to have or expecting to have life; \"the nerve is dead\"; \"a dead pallor\"; \"he was marked as a dead man by the assassin\") }\n\
      \{ asleep(p), at_peace2(p), at_rest(p), deceased, [ departed, noun.person:departed,+ ] gone, noun.communication:euphemism,;u (dead; \"he is deceased\"; \"our dear departed friend\") }\n\
      \{ assassinated, (murdered by surprise attack for political reasons; \"the 20th century has seen too many assassinated leaders\") }\n\
      \{ bloodless, exsanguine, exsanguinous, (destitute of blood or apparently so; \"the bloodless carcass of my Hector sold\"- John Dryden) }\n\
      \{ brain_dead, (having irreversible loss of brain function as indicated by a persistent flat electroencephalogram; \"was declared brain dead\") }\n\
      \{ [ breathless, noun.state:breathlessness,+ ] [ inanimate, noun.attribute:inanimateness,+ ] pulseless, (appearing dead; not breathing or having no perceptible pulse; \"an inanimate body\"; \"pulseless and dead\") }\n\
      \{ cold, (lacking the warmth of life; \"cold in his grave\") }\n\
      \{ d.o.a., (abbreviation for `dead on arrival' at the emergency room) }\n\
      \{ deathlike, deathly, (having the physical appearance of death; \"a deathly pallor\") }\n\
      \{ [ defunct, noun.state:defunctness,+ ] (having ceased to exist or live; \"the will of a defunct aunt\"; \"a defunct Indian tribe\") }\n\
      \{ [ doomed, noun.group:doomed,+ ] (marked for certain death; \"the black spot told the old sailor he was doomed\") }\n\
      \{ executed, (put to death as punishment; \"claimed the body of the executed traitor\") }\n\
      \{ fallen, (killed in battle; \"to honor fallen soldiers\") }\n\
      \{ late(a), (having died recently; \"her late husband\") }\n\
      \{ [ lifeless1, noun.attribute:lifelessness,+ ] exanimate, (deprived of life; no longer living; \"a lifeless body\") }\n\
      \{ murdered, (killed unlawfully; \"the murdered woman\"; \"lay a wreath on murdered Lincoln's bier\") }\n\
      \{ nonviable, (not capable of living or developing successfully) }\n\
      \{ slain1, (killed; `slain' is formal or literary as in \"slain warriors\"; \"a picture of St. George and the slain dragon\") }\n\
      \{ stillborn, ((of newborn infant) showing no signs of life at birth; not liveborn; \"a stillborn baby\") }\n\
      \{ stone-dead, (as lifeless as a stone) }]\n"
    ]

nounFiles = [ "noun.act"
            , "noun.animal"
            , "noun.artifact"
            , "noun.attribute"
            , "noun.body"
            , "noun.cognition"
            , "noun.communication"
            , "noun.event"
            , "noun.feeling"
            , "noun.food"
            , "noun.group"
            , "noun.location"
            , "noun.motive"
            , "noun.object"
            , "noun.person"
            , "noun.phenomenon"
            , "noun.plant"
            , "noun.possession"
            , "noun.process"
            , "noun.quantity"
            , "noun.relation"
            , "noun.shape"
            , "noun.state"
            , "noun.substance"
            , "noun.time"
            , "noun.Tops"
            ]

verbFiles = [ "verb.body"
            , "verb.change"
            , "verb.cognition"
            , "verb.communication"
            , "verb.competition"
            , "verb.consumption"
            , "verb.contact"
            , "verb.creation"
            , "verb.emotion"
            , "verb.motion"
            , "verb.perception"
            , "verb.possession"
            , "verb.social"
            , "verb.stative"
            , "verb.weather"
            ]

adverbFiles = [ "adv.all" ]

adjectiveFiles1 = [ "adj.pert"
                  , "adj.ppl"
                  ]

showResult :: (Show a) => Bool -> Result [a] -> IO ()
showResult doesshowresult er = do 
  case er of
    Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
    Partial f -> case (f "") of
                   Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
                   Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)
    Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)


process dir typ files = do
  flip mapM_ files $ \f -> do
    let fp = dir </> f
    putStrLn fp
    txt <- TIO.readFile fp
    let er = parse (many1 (p_synset typ)) txt
    showResult False er


main' = do
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/b/dbfiles"  
  let fp = dir </> "adj.all"
  putStrLn fp
  txt <- TIO.readFile fp
  let er = parse (many1 p_synset_adj_cluster) txt
  showResult True er

main = do
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/b/dbfiles"
  process dir Adjective adjectiveFiles1
  process dir Adverb    adverbFiles
  process dir Verb      verbFiles
  process dir Noun      nounFiles
  

test = do
  let txt = testdata_adj_cluster !! 1
      er = parse (many1 p_synset_adj_cluster_test) txt
  showResult True er 
