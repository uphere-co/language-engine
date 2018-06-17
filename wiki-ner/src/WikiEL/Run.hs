module WikiEL.Run where

import           Control.Lens                          ((^.),(^..))
import           Data.Maybe
--
import           Data.Text                             (Text)
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
import           System.FilePath                       ((</>))
import           WikiEL.EntityLinking
--
import           WikiEL.Type                           (EntityMention,ItemClass(..),beg,end)
import           WikiEL.Type.FileFormat
import qualified WikiEL.WikiEntityClass        as WC
import qualified WikiEL                        as WEL


prepareWNP :: [Sentence] -> [(Text,NamedEntityClass,POSTag)]
prepareWNP sents =
  let ws = catMaybes $ sents ^.. traverse . sentenceWord . traverse
      ns' = catMaybes $ sents ^.. traverse . sentenceNER . traverse
      ns = map (\x -> (fromMaybe (error (show x)) . classify) x) ns'
      ts = catMaybes $ sents ^.. traverse . sentenceToken . traverse
      ps = map (^. token_pos) ts
  in (zip3 ws ns ps)


mkConstraintFromWikiEL :: [EntityMention Text] -> [(Int,Int)]
mkConstraintFromWikiEL wikiel = map (\x -> let irange = entityIRange x in (irange ^. beg,irange ^. end)) $ wikiel

runEL :: ([(Text,NamedEntityClass,POSTag)] -> [EntityMention Text])
      -> ([EntityMention Text] -> [EntityMention Text])
      -> [Sentence]
      -> [EntityMention Text]
runEL tagger entityResolve sents  =
  let wnps = prepareWNP sents
      linked_mentions = tagger wnps
  in entityResolve linked_mentions


loadWikiData ::
       FilePath
    -> IO ( [(Text, NamedEntityClass,POSTag)] -> [EntityMention Text]
          , [EntityMention Text] -> [EntityMention Text])
loadWikiData dataDir = do
  edges  <- WEL.loadAndSortEdges (graphFilesG dataDir)
  uidTag <- WEL.loadFiles        (classFilesG dataDir)
  titles <- WEL.loadWikipageMapping (wikiTitleMappingFileG dataDir)
  tagger <- WEL.loadFEMtagger (reprFileG dataDir) (classFilesG dataDir)
  let entityResolve = WEL.disambiguateMentions edges uidTag titles
  return (tagger,entityResolve)


reprFileTinyG :: FilePath -> EntityReprFile
reprFileTinyG dataDir =
  EntityReprFile (dataDir </> "wiki-ner/data/wikidata.test.entities")


orgItemFileG,personItemFileG,brandItemFileG,locationItemFileG,occupationItemFileG,humanRuleItemFileG,buildingItemFileG :: FilePath-> ItemIDFile
orgItemFileG        dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.org")
personItemFileG     dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.person")
brandItemFileG      dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.brand")
locationItemFileG   dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.loc")
occupationItemFileG dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.occupation")
humanRuleItemFileG  dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.human_rule")
buildingItemFileG   dataDir = ItemIDFile (dataDir </> "wiki-ner/data/ne.building")


classFilesG :: FilePath -> [(ItemClass,ItemIDFile)]
classFilesG dataDir =
  [ (WC.personClass    , personItemFileG     dataDir)
  , (WC.orgClass       , orgItemFileG        dataDir)
  , (WC.brandClass     , brandItemFileG      dataDir)
  , (WC.occupationClass, occupationItemFileG dataDir)
  , (WC.locationClass  , locationItemFileG   dataDir)
  , (WC.humanRuleClass , humanRuleItemFileG  dataDir)
  , (WC.buildingClass  , buildingItemFileG   dataDir)
  ]

reprFileG :: FilePath -> EntityReprFile
reprFileG dataDir =
  EntityReprFile (dataDir </> "wiki-ner/data/names")

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
