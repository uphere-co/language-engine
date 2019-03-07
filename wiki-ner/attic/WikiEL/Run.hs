module WikiEL.Run where

import           Control.Lens                          ( (^.), (^..) )
import           Data.Maybe                            ( catMaybes, fromMaybe )
import           Data.Text                             ( Text )
import           System.FilePath                       ( (</>) )
------
import           NLP.Type.CoreNLP                      ( Sentence
                                                       , sentenceNER
                                                       , sentenceToken
                                                       , sentenceWord
                                                       , token_pos
                                                       )
import           NLP.Type.NamedEntity                  ( NamedEntityClass
                                                       , classify
                                                       )
import           NLP.Type.PennTreebankII               ( POSTag )
------
import           WikiEL.EntityLinking                  ( entityIRange )
import           WikiEL.Type                           ( EntityMention
                                                       , ItemClass(..)
                                                       , beg, end
                                                       )
import           WikiEL.Type.FileFormat                ( EntityReprFile(..)
                                                       , ItemIDFile(..)
                                                       , PropertyNameFile(..)
                                                       , WikiTitleMappingFile(..)
                                                       , WordNetMappingFile(..)
                                                       )
import qualified WikiEL.WikiEntityClass        as WC
-- import qualified WikiEL                        as WEL




mkConstraintFromWikiEL :: [EntityMention Text] -> [(Int,Int)]
mkConstraintFromWikiEL wikiel = map (\x -> let irange = entityIRange x in (irange ^. beg,irange ^. end)) $ wikiel


reprFileTinyG :: FilePath -> EntityReprFile
reprFileTinyG dataDir =
  EntityReprFile (dataDir </> "wiki-ner/data/wikidata.test.entities")





-- Full data

wikiTitleMappingFileG :: FilePath -> WikiTitleMappingFile
wikiTitleMappingFileG dataDir =
  WikiTitleMappingFile (dataDir </> "wiki-ner/wiki_id.page_title.txt")


wordnetMappingFileG :: FilePath -> WordNetMappingFile
wordnetMappingFileG dataDir =
  WordNetMappingFile (dataDir </> "wiki-ner/page_id.wiki_id.wordnet.tsv")


propertyNameFileG :: FilePath -> PropertyNameFile
propertyNameFileG dataDir =
  PropertyNameFile (dataDir </> "wiki-ner/properties.tsv")

listedCompanyFileG :: FilePath -> FilePath
listedCompanyFileG dataDir =
  dataDir </> "enwiki/companies"


graphFilesG :: FilePath -> FilePath
graphFilesG dataDir =
  dataDir </> "wiki-ner/interlinks.filtered"
