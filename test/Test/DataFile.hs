{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.DataFile where

import           WikiEL.Type.FileFormat


-- test data
rawNewsFile = "data/dao.ptb"
nerNewsFile = "data/dao.ner"

reprFileTiny = EntityReprFile "data/wikidata.test.entities"
orgItemFile = ItemIDFile "data/ne.org"
personItemFile = ItemIDFile "data/ne.person"
reprFile = EntityReprFile "data/uid"
-- Full data
propertyNameFile = PropertyNameFile "data_full/properties.tsv"
listedCompanyFile = "enwiki/companies"