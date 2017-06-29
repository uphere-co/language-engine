{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           WikiEL.Type.FileFormat


-- test data
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"
rawNewsFile2 = "data/iphone.ptb"
nerNewsFile2 = "data/iphone.ner"

reprFileTiny   = EntityReprFile "data/wikidata.test.entities"
orgItemFile    = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
brandItemFile  = ItemIDFile "data/ne.brand"
reprFile       = EntityReprFile "data/uid"

-- Full data
wordnetMappingFile   = WordNetMappingFile "data/page_id.wiki_id.wordnet.tsv"
propertyNameFile = PropertyNameFile   "data_full/properties.tsv"
listedCompanyFile= "enwiki/companies"
