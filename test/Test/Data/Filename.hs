{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           WikiEL.Type.FileFormat


-- news for testing
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"
rawNewsFile2 = "data/iphone.ptb"
nerNewsFile2 = "data/iphone.ner"
rawNewsFile3 = "data/article.amazon_nike.ptb"
nerNewsFile3 = "data/article.amazon_nike.ner"

-- test data
reprFileTiny   = EntityReprFile "data/wikidata.test.entities"
orgItemFile    = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
brandItemFile  = ItemIDFile "data/ne.brand"
locationItemFile = ItemIDFile "data/ne.loc"
occupationItemFile = ItemIDFile "data/ne.occupation"
--reprFile       = EntityReprFile "data/names"
--reprFile       = EntityReprFile "data/names.person.2"
reprFile       = EntityReprFile "data/names.pl.2"
-- Full data
wordnetMappingFile   = WordNetMappingFile "data/page_id.wiki_id.wordnet.tsv"
propertyNameFile = PropertyNameFile   "data_full/properties.tsv"
listedCompanyFile= "enwiki/companies"
