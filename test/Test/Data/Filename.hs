{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Data.Filename where

import           WikiEL.Type.FileFormat


-- test data
rawNewsFile = "/home/jihuni/repo/uphere/wiki-ner/data/dao.ptb"
nerNewsFile = "/home/jihuni/repo/uphere/wiki-ner/data/dao.ner"

reprFileTiny = EntityReprFile "/home/jihuni/repo/uphere/wiki-ner/data/wikidata.test.entities"
orgItemFile = ItemIDFile "/home/jihuni/repo/uphere/wiki-ner/data/ne.org"
personItemFile = ItemIDFile "/home/jihuni/repo/uphere/wiki-ner/data/ne.person"
reprFile = EntityReprFile "/home/jihuni/repo/uphere/wiki-ner/data/uid"
-- Full data
propertyNameFile = PropertyNameFile "/home/jihuni/repo/uphere/wiki-ner/data_full/properties.tsv"
listedCompanyFile = "/home/jihuni/repo/uphere/wiki-ner/enwiki/companies"