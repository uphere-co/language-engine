{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           WikiEL.Type.FileFormat
import qualified Test.Data.POS                 as POS


-- news for testing
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"
rawNewsFile2 = "data/iphone.ptb"
nerNewsFile2 = "data/iphone.ner"
rawNewsFile3 = "data/article.amazon_nike.ptb"
nerNewsFile3 = "data/article.amazon_nike.ner"
posNewsFile3 = POS.amazonNike
rawNewsFile4 = "data/article.health_bill.ptb"
nerNewsFile4 = "data/article.health_bill.ner"
posNewsFile4 = POS.healthBill
rawNewsFile5 = "data/article.china_trump.ptb"
nerNewsFile5 = "data/article.china_trump.ner"
posNewsFile5 = POS.chinaTrump


-- test data
reprFileTiny   = EntityReprFile "data/wikidata.test.entities"
orgItemFile    = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
brandItemFile  = ItemIDFile "data/ne.brand"
locationItemFile = ItemIDFile "data/ne.loc"
occupationItemFile = ItemIDFile "data/ne.occupation"
humanRuleItemFile = ItemIDFile "data/ne.human_rule"
buildingItemFile = ItemIDFile "data/ne.building"
reprFile       = EntityReprFile "data/names"
--reprFile       = EntityReprFile "data/names.1"
--reprFile       = EntityReprFile "data/names.person.2"
-- Full data
wordnetMappingFile= WordNetMappingFile "data/page_id.wiki_id.wordnet.tsv"
propertyNameFile  = PropertyNameFile   "data_full/properties.tsv"
listedCompanyFile = "enwiki/companies"
