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

                         


addSense :: IntMap [SenseItem] -> SenseItem -> IntMap [SenseItem]
addSense !m s = IM.insertWith (++) (s^.sense_soffset) [s] m

showResult :: (Show a) => Bool -> Result [a] -> IO ()
showResult doesshowresult er = do 
  case er of
    Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
    Partial f -> case (f "") of
                   Fail i xs err -> mapM_ print xs >> print err >> print (T.take 100 i)
                   Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)
    Done i r -> when doesshowresult (mapM_ print (Data.List.take 100 r)) >> print (length r) >> print (T.take 100 i)


  
main0 = do
  -- print (toEnum 44  :: LexicoGrapherFile)
  let dir = "/scratch/wavewave/wordnet/WordNet-3.1/dict"
  ss <- (catMaybes <$> parseFile parseSense (dir </> "index.sense"))
  mapM_ print . drop 10000 . Prelude.take 11000 $ ss


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
    ]


nounfiles = [ "noun.act"
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

verbfiles = [ "verb.body"
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

processNouns = do
  flip mapM_ nounfiles $ \f -> do
    let fp = "/scratch/wavewave/wordnet/WordNet-3.1/b/dbfiles" </> f
    putStrLn fp
    txt <- TIO.readFile fp
    let er = parse (many1 (p_synset Noun)) txt
    showResult False er

processVerbs = do
  flip mapM_ verbfiles $ \f -> do
    let fp = "/scratch/wavewave/wordnet/WordNet-3.1/b/dbfiles" </> f
    putStrLn fp
    txt <- TIO.readFile fp
    let er = parse (many1 (p_synset Verb)) txt
    showResult False er


main = do
  -- processNouns
  processVerbs
  
  

main' = do
  let txt = testdata_verb !! 4
  -- let txt = "carbon-14"

  let er = parse (many1 p_synset_test) txt

  showResult True er 
