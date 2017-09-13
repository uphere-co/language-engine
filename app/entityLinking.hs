module Main where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO


import qualified WikiEL.WikiNamedEntityTagger  as WNET
import qualified WikiEL                        as WEL
import           Test.Data.Filename
import           Test.Data.POS

runEL (tagger,entityResolve) nerFile posFile = do
  input_ner <- T.IO.readFile nerFile
  let
    ps = getPoStags posFile
    ns = WNET.loadStanfordNERoutput input_ner
    tokens = zip3 (WNET.getWords ns) (WNET.getNETags ns) ps
    
    linked_mentions = tagger tokens
    disambiguated_mentions = entityResolve linked_mentions

  mapM_ print disambiguated_mentions
  
main = do
  edges  <- WEL.loadAndSortEdges graphFiles
  uidTag <- WEL.loadFiles classFiles  
  titles <- WEL.loadWikipageMapping wikiTitleMappingFile
  tagger <- WEL.loadFEMtagger reprFile classFiles
  let
    entityResolve = WEL.disambiguateMentions edges uidTag titles
    run = runEL (tagger,entityResolve)

  run nerNewsFile3 posNewsFile3
  print "///////////////////////////////////"
  run nerNewsFile4 posNewsFile4
  print "///////////////////////////////////"
  run nerNewsFile5 posNewsFile5
