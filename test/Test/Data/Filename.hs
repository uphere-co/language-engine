{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           Data.Maybe                               (fromJust)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.Type.FileFormat
import qualified NLP.Type.NamedEntity          as N
import qualified WikiEL.WikiEntityClass        as WC
import qualified Test.Data.POS                 as POS


-- news for testing
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"
rawNewsFile2 = "data/iphone.ptb"
nerNewsFile2 = "data/iphone.ner"

-- https://www.bloomberg.com/news/articles/2017-06-21/amazon-said-to-sell-nike-shoes-directly-through-brand-registry
nerNewsFile3 = "data/article.amazon_nike.ner"
posNewsFile3 = POS.amazonNike
-- https://www.bloomberg.com/news/articles/2017-05-07/senate-republicans-plan-health-bill-that-keeps-some-f-obamacare
nerNewsFile4 = "data/article.health_bill.ner"
posNewsFile4 = POS.healthBill
-- https://www.bloomberg.com/news/articles/2017-08-24/china-says-u-s-probe-sabotages-existing-global-trade-system
nerNewsFile5 = "data/article.china_trump.ner"
posNewsFile5 = POS.chinaTrump

nerNewsSet1 = "data/bloomberg.ner"
posNewsSet1 = POS.bloombergSet
titleNewsSet1 = "data/bloomberg.title"


loadNEROutfile :: FilePath -> [(POS.POSTag, T.Text)] -> IO [(T.Text, N.NamedEntityClass, POS.POSTag)]
loadNEROutfile file poss = do
  text <- T.IO.readFile file
  let
    f tokenStr = (T.dropEnd 1 word', (fromJust . N.classify) tag)
      where (word',tag) = T.breakOnEnd (T.pack "/") tokenStr
    ns = map f (T.words text)
  return $ map (\((x,y),z) -> (x,y,z)) (zip ns (map fst poss))
    

-- test data
reprFileTiny   = EntityReprFile "data/wikidata.test.entities"
orgItemFile    = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
brandItemFile  = ItemIDFile "data/ne.brand"
locationItemFile = ItemIDFile "data/ne.loc"
occupationItemFile = ItemIDFile "data/ne.occupation"
humanRuleItemFile = ItemIDFile "data/ne.human_rule"
buildingItemFile = ItemIDFile "data/ne.building"

classFiles = [ (WC.personClass, personItemFile)
             , (WC.orgClass,    orgItemFile)
             , (WC.brandClass,  brandItemFile)
             , (WC.occupationClass, occupationItemFile)
             , (WC.locationClass,   locationItemFile)
             , (WC.humanRuleClass,  humanRuleItemFile)
             , (WC.buildingClass,   buildingItemFile)
             ]

reprFile       = EntityReprFile "data/names"
--reprFile       = EntityReprFile "data/names.1"
--reprFile       = EntityReprFile "data/names.person.2"
-- Full data
wikiTitleMappingFile= WikiTitleMappingFile "data/wiki_id.page_title.txt"
wordnetMappingFile= WordNetMappingFile "data/page_id.wiki_id.wordnet.tsv"
propertyNameFile  = PropertyNameFile   "data_full/properties.tsv"
listedCompanyFile = "enwiki/companies"

graphFiles = "enwiki/interlinks.filtered"

investopediaTermFile = "data/terms"
