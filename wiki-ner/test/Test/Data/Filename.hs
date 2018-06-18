{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           Data.Maybe                               (fromJust)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.Type                              (ItemClass)
import           WikiEL.Type.FileFormat
import qualified NLP.Type.NamedEntity          as N
import qualified WikiEL.WikiEntityClass        as WC
import qualified Test.Data.POS                 as POS


-- news for testing
rawNewsFile, nerNewsFile, rawNewsFile2, nerNewsFile2 :: FilePath
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"
rawNewsFile2 = "data/iphone.ptb"
nerNewsFile2 = "data/iphone.ner"

-- https://www.bloomberg.com/news/articles/2017-06-21/amazon-said-to-sell-nike-shoes-directly-through-brand-registry
nerNewsFile3 :: FilePath
nerNewsFile3 = "data/article.amazon_nike.ner"
posNewsFile3 :: [(POS.POSTag,T.Text)]
posNewsFile3 = POS.amazonNike
-- https://www.bloomberg.com/news/articles/2017-05-07/senate-republicans-plan-health-bill-that-keeps-some-f-obamacare

nerNewsFile4 :: FilePath
nerNewsFile4 = "data/article.health_bill.ner"

posNewsFile4 :: [(POS.POSTag,T.Text)]
posNewsFile4 = POS.healthBill
-- https://www.bloomberg.com/news/articles/2017-08-24/china-says-u-s-probe-sabotages-existing-global-trade-system
nerNewsFile5 :: FilePath
nerNewsFile5 = "data/article.china_trump.ner"

posNewsFile5 :: [(POS.POSTag,T.Text)]
posNewsFile5 = POS.chinaTrump

nerNewsSet1, titleNewsSet1 :: FilePath
nerNewsSet1 = "data/bloomberg.ner"
titleNewsSet1 = "data/bloomberg.title"

posNewsSet1 :: [(POS.POSTag,T.Text)]
posNewsSet1 = POS.bloombergSet


nerNewsSet2, titleNewsSet2 :: FilePath
nerNewsSet2 = "data/bloomberg2.ner"
titleNewsSet2 = "data/bloomberg2.title"

posNewsSet2 :: [(POS.POSTag,T.Text)]
posNewsSet2 = POS.bloombergSet2


loadNEROutfile :: FilePath -> [(POS.POSTag, T.Text)] -> IO [(T.Text, N.NamedEntityClass, POS.POSTag)]
loadNEROutfile file poss = do
  text <- T.IO.readFile file
  let
    f tokenStr = (T.dropEnd 1 word', (fromJust . N.classify) tag)
      where (word',tag) = T.breakOnEnd (T.pack "/") tokenStr
    ns = map f (T.words text)
  return $ map (\((x,y),z) -> (x,y,z)) (zip ns (map fst poss))



orgItemFile, personItemFile, brandItemFile, locationItemFile, occupationItemFile, humanRuleItemFile, buildingItemFile :: ItemIDFile
orgItemFile    = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
brandItemFile  = ItemIDFile "data/ne.brand"
locationItemFile = ItemIDFile "data/ne.loc"
occupationItemFile = ItemIDFile "data/ne.occupation"
humanRuleItemFile = ItemIDFile "data/ne.human_rule"
buildingItemFile = ItemIDFile "data/ne.building"

classFiles :: [(ItemClass,ItemIDFile)]
classFiles = [ (WC.personClass, personItemFile)
             , (WC.orgClass,    orgItemFile)
             , (WC.brandClass,  brandItemFile)
             , (WC.occupationClass, occupationItemFile)
             , (WC.locationClass,   locationItemFile)
             , (WC.humanRuleClass,  humanRuleItemFile)
             , (WC.buildingClass,   buildingItemFile)
             ]

-- test data
reprFileTiny, reprFile :: EntityReprFile
reprFileTiny   = EntityReprFile "data/wikidata.test.entities"
reprFile       = EntityReprFile "data/names"
--reprFile       = EntityReprFile "data/names.1"
--reprFile       = EntityReprFile "data/names.person.2"
-- Full data
wikiTitleMappingFile :: WikiTitleMappingFile
wikiTitleMappingFile= WikiTitleMappingFile "data/wiki_id.page_title.txt"

wordnetMappingFile :: WordNetMappingFile
wordnetMappingFile= WordNetMappingFile "data/page_id.wiki_id.wordnet.tsv"

propertyNameFile :: PropertyNameFile
propertyNameFile  = PropertyNameFile   "data_full/properties.tsv" -- obsoleted. Not used for now.

listedCompanyFile :: FilePath
listedCompanyFile = "enwiki/companies" -- obsoleted. Not used for now.

graphFiles :: FilePath
graphFiles = "data/interlinks.filtered"

investopediaTermFile :: FilePath
investopediaTermFile = "data/terms"
